"""Download + install the volca binary + reference data bundle.

Public surface: :func:`download`. ``Server`` calls ``installed_binary`` and
``installed_data_dir`` to pick up artefacts when no explicit binary path is
configured.

The install layout is shared with ``install.sh`` and ``install.ps1`` —
running any of the three installers populates the same root::

    <platformdirs.user_data_dir("volca", appauthor=False)>/
        <engine-version>/volca[.exe]
        data/<data-version>/{flows.csv, compartments.csv, units.csv,
                             geographies.csv, flows.csv.cache.zst}
        data/current   -> data/<data-version>   (symlink, or copy on
                                                 platforms without symlinks)

Resolved per-platform:
    - Linux:   ${XDG_DATA_HOME:-~/.local/share}/volca/
    - macOS:   ~/Library/Application Support/volca/
    - Windows: %LOCALAPPDATA%\\volca\\

Override the root with ``$VOLCA_HOME`` (full path, skips platform detection).
"""

from __future__ import annotations

import contextlib
import hashlib
import json
import os
import platform
import re
import sys
import tarfile
import urllib.request
import zipfile
from dataclasses import dataclass
from pathlib import Path
from typing import Iterator, Optional

from platformdirs import user_data_dir

if sys.platform == "win32":
    import msvcrt
else:
    import fcntl

_REPO_DEFAULT = "ccomb/volca"
_GH_API = "https://api.github.com"
_GH_RELEASES = "https://github.com/{repo}/releases/download/{tag}/{asset}"


class DownloadError(RuntimeError):
    """Raised when the download or verification fails."""


@dataclass(frozen=True)
class Installed:
    """Result of :func:`download`."""

    binary: Path
    data_dir: Path
    version: str
    data_version: str


# ---------------------------------------------------------------------------
# Platform detection
# ---------------------------------------------------------------------------


def _platform_slug() -> tuple[str, str]:
    """Return (slug, asset_ext) — e.g. ('linux-amd64', 'tar.gz')."""
    sysname = platform.system()
    machine = platform.machine().lower()
    if sysname == "Linux":
        if machine in ("x86_64", "amd64"):
            return "linux-amd64", "tar.gz"
        if machine in ("aarch64", "arm64"):
            return "linux-arm64", "tar.gz"
    elif sysname == "Darwin":
        if machine in ("arm64", "aarch64"):
            return "macos-arm64", "tar.gz"
    elif sysname == "Windows":
        if machine.lower() in ("amd64", "x86_64", "x64"):
            return "windows-amd64", "zip"
    raise DownloadError(f"unsupported platform: {sysname}/{machine}")


def _binary_name() -> str:
    return "volca.exe" if platform.system() == "Windows" else "volca"


# ---------------------------------------------------------------------------
# Install paths
# ---------------------------------------------------------------------------

_SEMVER_RE = re.compile(r"^(\d+)\.(\d+)\.(\d+)$")


def _install_root() -> Path:
    """Resolve the volca install root.

    ``$VOLCA_HOME`` overrides everything (full path). Otherwise falls back
    to ``platformdirs.user_data_dir("volca", appauthor=False)`` — same root
    as install.sh / install.ps1.
    """
    if home := os.environ.get("VOLCA_HOME"):
        return Path(home)
    return Path(user_data_dir("volca", appauthor=False))


def _manifest_path() -> Path:
    return _install_root() / "latest.json"


def _scan_installed_versions(root: Path) -> list[tuple[tuple[int, int, int], Path]]:
    """Find every ``<root>/X.Y.Z/<binary>`` and rank by semver, highest first.

    Lets ``installed_binary()`` discover engines installed via install.sh /
    install.ps1, which don't write ``latest.json``.
    """
    if not root.is_dir():
        return []
    binary = _binary_name()
    found: list[tuple[tuple[int, int, int], Path]] = []
    for child in root.iterdir():
        if not child.is_dir():
            continue
        m = _SEMVER_RE.match(child.name)
        if not m:
            continue
        bin_path = child / binary
        if not bin_path.is_file():
            continue
        key = (int(m.group(1)), int(m.group(2)), int(m.group(3)))
        found.append((key, bin_path))
    found.sort(reverse=True)
    return found


def installed_binary(version: Optional[str] = None) -> Optional[Path]:
    """Return the binary path for ``version`` if extracted, else ``None``.

    With ``version=None``, prefers the version recorded in ``latest.json``
    by the most recent :func:`download` call. If that file is missing — e.g.
    the user ran install.sh / install.ps1, which don't write a manifest —
    falls back to scanning the install root for the highest-semver dir that
    contains the binary.
    """
    root = _install_root()
    if version is not None:
        p = root / version.lstrip("v") / _binary_name()
        return p if p.is_file() else None
    manifest = _manifest_path()
    if manifest.is_file():
        try:
            data = json.loads(manifest.read_text())
            ver = data.get("version")
            if ver:
                p = root / ver.lstrip("v") / _binary_name()
                if p.is_file():
                    return p
        except (OSError, json.JSONDecodeError):
            pass
    scanned = _scan_installed_versions(root)
    return scanned[0][1] if scanned else None


def installed_data_dir() -> Optional[Path]:
    """Return the active data dir (``data/current``) if it exists.

    Resolves symlinks before returning, so a stale link to a removed
    target reports None instead of pointing the engine at a path it
    cannot read. ``Path.exists`` follows symlinks; ``is_symlink`` alone
    would happily return a broken pointer.
    """
    p = _install_root() / "data" / "current"
    if p.exists() and p.is_dir():
        return p
    return None


# ---------------------------------------------------------------------------
# GH Releases API + asset fetching
# ---------------------------------------------------------------------------


def _http_get(url: str, accept: str = "application/octet-stream") -> bytes:
    """Plain GET. No auth — releases are public. Token lifted from env if
    present (avoids public-API rate limits in CI loops)."""
    req = urllib.request.Request(url, headers={"Accept": accept})
    token = os.environ.get("GITHUB_TOKEN")
    if token:
        req.add_header("Authorization", f"Bearer {token}")
    with urllib.request.urlopen(req) as resp:
        return resp.read()


def _resolve_tag(repo: str, version: Optional[str]) -> str:
    if version:
        return version if version.startswith("v") else f"v{version}"
    payload = json.loads(_http_get(f"{_GH_API}/repos/{repo}/releases/latest", "application/vnd.github+json"))
    tag = payload.get("tag_name")
    if not tag:
        raise DownloadError(f"could not resolve latest release tag for {repo}")
    return tag


def _download_asset(repo: str, tag: str, asset: str, dest: Path) -> None:
    url = _GH_RELEASES.format(repo=repo, tag=tag, asset=asset)
    dest.parent.mkdir(parents=True, exist_ok=True)
    dest.write_bytes(_http_get(url))


def _parse_sha256sums(text: str) -> dict[str, str]:
    """`sha256sum`-format → {filename: hex digest}."""
    out: dict[str, str] = {}
    for line in text.splitlines():
        m = re.match(r"^([0-9a-fA-F]{64})\s+\*?(.+)$", line.strip())
        if m:
            out[m.group(2)] = m.group(1).lower()
    return out


def _verify(path: Path, expected_hex: str) -> None:
    h = hashlib.sha256()
    with path.open("rb") as f:
        for chunk in iter(lambda: f.read(1 << 16), b""):
            h.update(chunk)
    actual = h.hexdigest()
    if actual != expected_hex.lower():
        raise DownloadError(f"SHA256 mismatch for {path.name}: expected {expected_hex}, got {actual}")


def _extract(archive: Path, dest: Path) -> None:
    dest.mkdir(parents=True, exist_ok=True)
    if archive.suffix == ".zip":
        with zipfile.ZipFile(archive) as zf:
            zf.extractall(dest)
    else:
        with tarfile.open(archive, "r:gz") as tf:
            tf.extractall(dest)


@contextlib.contextmanager
def _exclusive_lock(path: Path) -> Iterator[None]:
    """Hold a process-exclusive lock on ``path`` for the duration of the block.

    Lets concurrent ``download()`` callers serialise around the install
    writes: one process actually downloads + extracts, the others wait, and
    on re-entering the critical section see a fully-populated root and
    short-circuit. The OS releases the lock if the holder dies, so a crashed
    run does not strand subsequent callers.

    fcntl.flock on Unix; msvcrt.locking on Windows. Both are kernel-level.
    """
    path.parent.mkdir(parents=True, exist_ok=True)
    fd = os.open(str(path), os.O_RDWR | os.O_CREAT, 0o644)
    try:
        if sys.platform == "win32":
            # LK_LOCK retries every 1s up to 10× then raises. Wrap in our own
            # loop so we wait indefinitely without surfacing OSError to the
            # caller — this lock is intentionally blocking.
            while True:
                try:
                    msvcrt.locking(fd, msvcrt.LK_LOCK, 1)
                    break
                except OSError:
                    continue
            try:
                yield
            finally:
                os.lseek(fd, 0, os.SEEK_SET)
                msvcrt.locking(fd, msvcrt.LK_UNLCK, 1)
        else:
            fcntl.flock(fd, fcntl.LOCK_EX)
            try:
                yield
            finally:
                fcntl.flock(fd, fcntl.LOCK_UN)
    finally:
        os.close(fd)


def _link_current(target: Path, link: Path) -> None:
    """Make ``link`` point at ``target``. Symlink where supported, plain
    copy fallback for Windows configurations without developer mode."""
    import shutil

    if link.is_symlink() or link.is_file():
        link.unlink()
    elif link.is_dir():
        shutil.rmtree(link)
    try:
        # Use a relative target so the install dir stays portable if the
        # parent gets moved (e.g. user's $HOME relocates).
        link.symlink_to(target.name, target_is_directory=True)
    except (OSError, NotImplementedError):
        shutil.copytree(target, link)


# ---------------------------------------------------------------------------
# Public entry point
# ---------------------------------------------------------------------------


def download(
    version: Optional[str] = None,
    repo: str = _REPO_DEFAULT,
    *,
    force: bool = False,
) -> Installed:
    """Download the volca binary + data bundle for the current platform.

    Idempotent: if both artefacts are already extracted under the install
    root and ``force=False``, returns immediately without network.

    Args:
        version: GH Release tag (``v0.7.0``); ``None`` resolves the latest.
        repo: GitHub repo slug. Default ``ccomb/volca``.
        force: Re-download even if the install root looks complete.

    Returns:
        :class:`Installed` with the resolved paths and versions.
    """
    slug, ext = _platform_slug()
    tag = _resolve_tag(repo, version)
    plain_version = tag.lstrip("v")

    root = _install_root()
    root.mkdir(parents=True, exist_ok=True)

    with _exclusive_lock(root / ".lock"):
        return _download_locked(root, repo, tag, plain_version, slug, ext, force)


def _download_locked(
    root: Path,
    repo: str,
    tag: str,
    plain_version: str,
    slug: str,
    ext: str,
    force: bool,
) -> Installed:
    # ---- download SHA256SUMS first; it tells us which data version to fetch
    sums_path = root / f"SHA256SUMS-{tag}"
    sums_path.write_bytes(_http_get(_GH_RELEASES.format(repo=repo, tag=tag, asset="SHA256SUMS")))
    sums = _parse_sha256sums(sums_path.read_text())

    binary_asset = f"volca-{plain_version}-{slug}.{ext}"
    if binary_asset not in sums:
        raise DownloadError(f"release {tag} has no asset {binary_asset}")

    data_assets = [n for n in sums if n.startswith("volca-data-") and n.endswith(".tar.gz")]
    if not data_assets:
        raise DownloadError(f"release {tag} has no volca-data-*.tar.gz asset")
    data_asset = data_assets[0]
    m = re.match(r"^volca-data-(.+)\.tar\.gz$", data_asset)
    if not m:
        raise DownloadError(f"cannot parse data version from {data_asset}")
    data_version = m.group(1)

    binary_dir = root / plain_version
    data_dir = root / "data" / data_version
    bin_path = binary_dir / _binary_name()

    already_installed = (
        bin_path.is_file()
        and (data_dir / "flows.csv").is_file()
        and not force
    )
    if not already_installed:
        # ---- binary
        bin_arch = root / f"_dl-{binary_asset}"
        _download_asset(repo, tag, binary_asset, bin_arch)
        _verify(bin_arch, sums[binary_asset])
        _extract(bin_arch, binary_dir)
        bin_arch.unlink(missing_ok=True)
        if sys.platform != "win32":
            os.chmod(bin_path, 0o755)

        # ---- data bundle
        data_arch = root / f"_dl-{data_asset}"
        _download_asset(repo, tag, data_asset, data_arch)
        _verify(data_arch, sums[data_asset])
        _extract(data_arch, data_dir)
        data_arch.unlink(missing_ok=True)

    _link_current(data_dir, root / "data" / "current")

    # Manifest lets Server.start() find the binary without knowing which
    # version was downloaded. install.sh / install.ps1 don't write this file
    # — installed_binary() falls back to a semver scan in that case.
    _manifest_path().write_text(
        json.dumps(
            {"version": plain_version, "data_version": data_version, "binary": str(bin_path)},
            indent=2,
        )
    )

    return Installed(
        binary=bin_path,
        data_dir=root / "data" / "current",
        version=plain_version,
        data_version=data_version,
    )
