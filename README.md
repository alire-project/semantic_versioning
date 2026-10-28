[![build](https://github.com/alire-project/semantic_versioning/workflows/build/badge.svg)](https://github.com/alire-project/semantic_versioning/actions)

# Semantic_Versioning

Semantic Versioning for the Ada language

Implements the 2.0 specification found at http://semver.org/

## Types

Three types are provided:

- `Semantic_Versioning.Version`, which stores a single semantic version (e.g., `"1.0.2-beta+build0bdf3"`).

- `Semantic_Versioning.Basic.Version_Set`, which stores a single version restriction over the whole set of versions (e.g., `"~1.2"`), or a list of AND (`&`) restrictions (e.g., `"^1.0 & /=1.1"`).

- `Semantic_Versioning.Extended.Version_Set`, which stores a
general subset expressed with combinations of AND (`&`) and OR (`|`) logical operators. As
in Ada, both cannot be mixed at the same level but can be combined with parentheses, e.g., `"(≥7 & ≤9) | ≥2018"`.

## Parsing

Versions are parsed strictly by default (`Parse`, `New_Version`, `V`, `+`):
anything not complying with the specification raises `Malformed_Input`, e.g.
leading zeros or characters other than `[0-9A-Za-z-]` in pre-release and build
identifiers.

Relaxed parsing (`Relaxed`, or `Parse` with `Relaxed => True`) accepts
versions as found in the wild, e.g. reported by package managers. Whatever
does not conform goes into the pre-release or build metadata, which are then
made compliant (`Compliant => True`, the default), so that the image of the
result is accepted by strict parsing: other characters become `-`, empty
identifiers are dropped, and numeric pre-release identifiers lose their
leading zeros. For example, `3.8.13_2` gives `3.8.13+-2`. With
`Compliant => False`, those parts keep the original text (`3.8.13+_2`), as
before version 4.0.

## Limitations

1. Wildcard version sets (`1.*`) are not yet supported, but for a single `*` which means "any version".

1. The special interpretation of caret and tilde for pre-1 versions that exists in some implementations is not applied, so they retain their usual meaning (compatibility within major/minor numbers). According to the Semver 2.0 spec, pre-1 releases do not offer any compatibility guarantees.

## Unicode

By default, relational operators are accepted as plain ASCII sequences (`<=, >=, /=`) and as
Unicode-encoded characters (`≤, ≥, ≠`). Unicode alternatives can be disabled in calls that
accept textual input or produce corresponding output.

## Usage

The public specifications are relatively small and intuitive.
Check the `tests/src` directory for examples of use.
