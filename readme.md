# PortableStructs.jl

This package provides an easy way to write out structs as YAML/JSON and also to load YAML/JSON and populate the appropriate struct.

It is easy to write (most) structs-of-structs out to a YAML file:

```
import PortableStructs
PortableStructs.write_to_yaml("file.yaml", my_struct)
```

It is similarly easy to load from YAML:

```
my_struct = PortableStructs.load_from_yaml("file.yaml")
```

The loaded structure will in general have the same native Julia types as the original.

Where the type you wish to load as is known, that can be provided:

```
my_struct = PortableStructs.load_from_yaml("file.yaml", MyType)
```

Here's an example.

```
@enum Status DoingWell DoingPoorly
@kwdef struct Position{T}
    x::T
    y::T
    z::T
end
@kwdef struct MyType
    name::String
    position::Position{Float64}
    status::Status
end

x = MyType("My Name", Position(1., 2., 3.), DoingWell)

import PortableStructs
PortableStructs.write_to_yaml("my_struct.yaml", x)
```

Here's what the YAML looks like:

```
type: "MyType"
name: "My Name"
position:
  type: "Position"
  x: 1.0
  y: 2.0
  z: 3.0
status: "DoingWell"
```

We can load that back in like so:

```
y = PortableStructs.load_from_yaml("my_struct.yaml")
```

giving:

```
MyType("My Name", Position{Float64}(1.0, 2.0, 3.0), DoingWell)
```

This package is meant to be simple, and that simplicity comes from several constraints:

* The user's structs will be constructed entirely from keyword arguments, one for each field, so they must have constructors that support this (such as by adding `@kwdef` in front of the struct definition).
* The type of each struct will show up in the YAML file with a key called "type" (or whatever string is specified by the `type_key` keyword argument to `write_to_yaml` and `load_from_yaml`). Hence no struct is allowed have a field with this name.
* A YAML file can be "included" at any level. This allows the user to break up a large YAML file into smaller ones. By default, the key `include` will be used to indicate what file to include. The `include_key` keyword argument to `load_from_yaml` can specify a different key to use (e.g., `_include`). When including files, the file name is assumed to be relative to the file that has the "include" in it (or an absolute path).
* This isn't meant to be fast or efficient.

There is overlap with the functionality in StructTypes. This package is not as flexible as that one, but it's simpler to make an arbitrary struct work with this package (generally, the user need not do anything at all) than with StructTypes, even for fields with abstract types.

Design notes:

* We could potentially put `load_from_yaml` in a YAML extension, `load_from_json` in a JSON extension, etc.
* The key reason this package exists, instead of just using StructTypes, is that this handles abstract types where the potential subtypes of the abstract type aren't known (one can't write a `StructTypes.subtypes` function to resolve which abstract type should be constructed).
