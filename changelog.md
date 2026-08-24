# PortableStructs.jl Change Log

## v1.4.0

* Added support for all integer, float, and char types.
* Added `from_dict` to load Tuple from a vector in an "args" key.
* Clarified error when general construction of the target type fails.
* Fixed a bug where keys did not preserve their order through an `include`.

## v1.3.0

* Added support for construction from non-conrete types, like `UnitRange` (instead of `UnitRange{Int64}`).
* Removed (now unnecessary) extension for `Random`.
* Added compatibility for `OrderedCollections` v2.0.
* Added behavior to resolve types in modules like was already done for enums.
* Added behavior to look for modules that aren't defined in `base_module` in `Main`.

## v1.2.0

* Added support for enums in nested modules. All enums are now written with explicit module paths like `ModuleA.SubModule1.Fruits.mango`.

## v1.1.0

* Added support for reading and writing the `Matrix` type.

## v1.0.0

* Added typed structural loading and writing through `from_dict` and `to_dict`.
* Added YAML and JSON support through package extensions.
* Added include support for splitting large input files across smaller files.
* Added extension adapters for `Random.Xoshiro` and `StaticArrays.SVector`.
