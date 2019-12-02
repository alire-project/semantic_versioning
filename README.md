[![Linux CI](https://github.com/alire-project/semantic_versioning/workflows/CI%20linux/badge.svg)](https://github.com/alire-project/semantic_versioning/actions)
[![Windows CI](https://github.com/alire-project/semantic_versioning/workflows/CI%20Windows/badge.svg)](https://github.com/alire-project/semantic_versioning/actions)

# Semantic_Versioning

Semantic Versioning for the Ada language

Implements the 2.0 specification found at http://semver.org/

## Types

Three types are provided:

- `Semantic_Versioning.Version`, which stores a single semantic version (e.g., `"1.0.2-beta+build0bdf3"`).

- `Semantic_Versioning.Version_Set` (aka *Basic Version Set*), which stores a single version restriction over the whole set of versions (e.g., `"~1.2"`), or a list of *and* restrictions (e.g., `"^1.0,/=1.1"`).

- `Semantic_Versioning.Extended.Version_Set` (aka *Extended Version Set*), which stores a 
general subset expressed with combinations of AND (`&`) and OR (`|`) logical operators. As
in Ada, both cannot be mixed at the same level but can be combined with parentheses, e.g., `"(≥7 & ≤9) | ≥2018"`.

## Limitations

1. Wildcard version sets (`1.*`) are not yet supported, but for a single `*` which means "any version".

1. The special interpretation of caret and tilde for pre-1 versions that exists in some implementations is not applied.

## A note on Unicode

By default, relational operators are accepted as plain ASCII sequences (`<=, >=, /=`) and as
Unicode-encoded characters (`≤, ≥, ≠`). Unicode alternatives can be disabled in calls that 
accept textual input or produce corresponding output.

## Usage

The public specifications are relatively small and intuitive. 
Check `semantic_versioning-demo.adb` for examples of use.
