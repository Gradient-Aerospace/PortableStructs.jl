# PortableStructs.jl Change Log

## Unreleased

* Added support for construction from non-conrete types, like `UnitRange` (instead of `UnitRange{Int64}`).

## v1.2.0

* Added support for enums in nested modules. All enums are now written with explicit module paths like `ModuleA.SubModule1.Fruits.mango`.

## v1.1.0

* Added support for reading and writing the `Matrix` type.

## v1.0.0

* Added typed structural loading and writing through `from_dict` and `to_dict`.
* Added YAML and JSON support through package extensions.
* Added include support for splitting large input files across smaller files.
* Added extension adapters for `Random.Xoshiro` and `StaticArrays.SVector`.
