; =============================================================================
; VoLCA NSIS Installer Hooks
; =============================================================================
; Installs the MS-MPI runtime silently if not already present.
;
; The msmpisetup.exe redistributable is embedded in the installer archive
; and extracted to a temporary directory at install time.
;
; BUILD REQUIREMENT: msmpisetup.exe must exist in desktop/resources/
;   The build script (build-desktop.sh) downloads it automatically.
; =============================================================================

!ifndef MSMPISETUP_PATH
  ; Default: same directory as the .nsh file (desktop/)
  !define MSMPISETUP_PATH "${__FILEDIR__}\resources\msmpisetup.exe"
!endif

!macro NSIS_HOOK_PREINSTALL
  ; Check if msmpi.dll is already available on the system
  IfFileExists "$SYSDIR\msmpi.dll" SkipMsMpi

  DetailPrint "Installing Microsoft MPI runtime..."
  SetOutPath "$PLUGINSDIR"
  File "${MSMPISETUP_PATH}"
  nsExec::ExecToLog '"$PLUGINSDIR\msmpisetup.exe" -unattend'
  Pop $0
  DetailPrint "MS-MPI installer returned: $0"
  SetOutPath "$INSTDIR"

  SkipMsMpi:
!macroend
