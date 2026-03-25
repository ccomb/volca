# pyvolca

Python client for [VoLCA](https://github.com/ccomb/volca), a Life Cycle Assessment engine.

## Install

```bash
pip install pyvolca
```

## Quick start

```python
from volca import Client

c = Client(db="agribalyse-3.2", password="1234")

# Search activities
plants = c.search_activities(name="at plant")

# Supply chain
chain = c.get_supply_chain(plants[0].process_id, name="at farm")

# LCIA with substitutions
subs = [{"from": old_pid, "to": new_pid, "consumer": consumer_pid}]
result = c.get_lcia(pid, method_id, substitutions=subs)
```

## License

MIT
