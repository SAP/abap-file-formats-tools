"""Validate live-generated IN schemas with jsonschema, without default insertion.

Prerequisites: transpile, run node test/conditions-in.mjs, install jsonschema.
Run: python test/conditions-in.py
"""
import json
from pathlib import Path

from jsonschema import Draft202012Validator


def validator(name):
    schema = json.loads(Path(f"generated/{name}.schema.json").read_text())
    Draft202012Validator.check_schema(schema)
    return Draft202012Validator(schema)


flat = validator("ty_cond_in")
nested = validator("ty_cond_in_nested")
cases = [
    ({"enumDetail": ""}, True),
    ({}, False),  # Omitted mode uses the declared matching default.
    ({"mode": "cloudDevelopment", "enumDetail": ""}, True),
    ({"mode": "cloudDevelopment"}, False),
    ({"mode": "keyUser"}, True),
    ({"mode": "unknown"}, False),
    ({"mode": "keyUser", "detail": ""}, False),
    ({"mode": "keyUser", "kind": "static", "detail": ""}, True),
    ({"mode": "keyUser", "kind": "dynamic", "detail": ""}, True),
    ({"mode": "keyUser", "kind": "static"}, False),
    ({"mode": "keyUser", "kind": "other"}, True),
    ({"mode": "keyUser", "kind": "other", "detail": ""}, False),
    ({"mode": "keyUser", "disabledDetail": ""}, True),
    ({"mode": "keyUser", "enabled": False, "disabledDetail": ""}, True),
    ({"mode": "keyUser", "enabled": True, "disabledDetail": ""}, False),
    ({"mode": "keyUser", "count": -1}, False),
    ({"mode": "keyUser", "count": 0, "counted": ""}, True),
    ({"mode": "keyUser", "count": 2, "counted": ""}, True),
    ({"mode": "keyUser", "count": 3}, True),
    ({"mode": "keyUser", "kind": "O'Brien, (a)", "quoted": ""}, True),
    ({"mode": "keyUser", "kind": "$requiredWhen", "quoted": ""}, True),
    ({"mode": "keyUser", "kind": "other", "quoted": ""}, False),
    ({"mode": "keyUser", "unknown": ""}, False),
    ({"mode": "keyUser", "kind": "static", "detail": None}, False),
]
for document, expected in cases:
    assert flat.is_valid(document) == expected, document

scope_cases = [
    ({}, True),  # Optional containing structures stay optional.
    ({"detail": ""}, False),  # Missing settings do not acquire boolean defaults.
    ({"settings": {"mode": "keyUser"}, "detail": ""}, True),
    ({"settings": {}}, False),
    ({"rows": [{"mode": "keyUser", "kind": "static", "detail": ""},
               {"mode": "keyUser", "kind": "other"}]}, True),
    ({"rows": [{"mode": "keyUser", "kind": "static"},
               {"mode": "keyUser", "kind": "other", "detail": ""}]}, False),
    ({"otherRows": [{"mode": "keyUser", "kind": "dynamic", "detail": ""}]}, True),
]
for document, expected in scope_cases:
    assert nested.is_valid(document) == expected, document

print(f"Passed {len(cases) + len(scope_cases)} draft-2020-12 validation cases")
