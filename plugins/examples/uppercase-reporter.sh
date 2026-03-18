#!/usr/bin/env bash
# Example shell reporter plugin for VoLCA.
#
# Demonstrates the JSON stdin/stdout protocol for reporter plugins.
# Reads a JSON request on stdin, returns transformed JSON on stdout.
#
# Protocol:
#   Input:  {"action": "report", "data": <any JSON value>}
#   Output: <any JSON value> (the formatted report)
#
# This trivial example upper-cases all string values in the JSON.
#
# Configure in volca.toml:
#   [[plugin]]
#   name = "uppercase"
#   type = "reporter"
#   path = "plugins/examples/uppercase-reporter.sh"
#   format-id = "upper"
#   mime-type = "application/json"

# Read JSON from stdin, use jq to uppercase all string values
# Falls back to cat if jq is not available
if command -v jq &>/dev/null; then
    jq '.. | strings |= ascii_upcase'
else
    # Without jq, just pass through
    cat
fi
