# PortableStructs.jl

This package provides an easy way to write out structs as YAML/JSON and also to load YAML/JSON and populate the appropriate struct.

PortableStructs is intended for trusted configuration and data files. Loading a typed file resolves type/function names from Julia modules and can call constructors or functions, so it should not be used as a safe deserializer for untrusted input.

It is easy to write (most) structs-of-structs out to a YAML file:

```
import PortableStructs
import YAML # Activates PortableStructs' YAML extension.
PortableStructs.write_to_yaml("file.yaml", my_struct)
```

It is similarly easy to load from YAML:

```
import PortableStructs
import YAML
my_struct = PortableStructs.load_from_yaml("file.yaml")
```

The loaded structure will in general have the same native Julia types as the original.

Where the type you wish to load as is known, that can be provided:

```
import PortableStructs
import YAML
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
import YAML
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
import PortableStructs
import YAML
y = PortableStructs.load_from_yaml("my_struct.yaml")
```

giving:

```
MyType("My Name", Position{Float64}(1.0, 2.0, 3.0), DoingWell)
```

The `type` can be a type or a function to call with keyword arguments.

The same dictionary representation can be written to JSON. The JSON methods are available once `JSON` is loaded:

```
import PortableStructs
import JSON

PortableStructs.write_to_json("my_struct.json", x)
y = PortableStructs.load_from_json("my_struct.json")
```

If the desired output type is known, pass it as the second argument:

```
y = PortableStructs.load_from_json("my_struct.json", MyType)
```

A YAML file can be "included" at any level. This allows the user to break up a large YAML file into smaller ones. By default, the key `include` will be used to indicate what file to include. The `include_key` keyword argument to `load_from_yaml` can specify a different key to use (e.g., `_include`). When including files, the file name is assumed to be relative to the file that has the "include" in it (or an absolute path).

Includes can also provide `except` entries to overwrite values from the included file. Exception paths use dot-separated dictionary keys. They can also target existing vector elements with 1-based indices, matching Julia's indexing. For example, `trees[2].common_name` overwrites the `common_name` key in the second element of the `trees` vector. Vector indices must already exist; exceptions do not append to vectors or create missing vector entries.

For example, `trees.yaml` might provide shared data:

```yaml
trees:
  - scientific_name: Arbutus unedo
    common_name: strawberry tree
  - scientific_name: Arbutus menziesii
    common_name: Pacific madrone
notes: Needs review.
```

Another file can include it and overwrite selected values:

```yaml
include:
  source: trees.yaml
  except:
    - path: trees[2].common_name
      value: madrona
    - path: notes
      value: Reviewed.
```

Loading the second file produces the included data with the second tree's common name changed and the top-level `notes` value replaced. The `trees[2]` path uses Julia-style 1-based indexing.

This package is meant to be simple, and that simplicity comes from several constraints:

* The user's structs will be constructed either from keyword arguments or from positional arguments. For positional arguments, the YAML/JSON file should have a key matching each field name, and the arguments will be provided to the constructor in the order of the field names (*not* in the order in which they're encountered in the YAML/JSON file).
* The type of each struct will show up in the YAML file with a key called "type" (or whatever string is specified by the `type_key` keyword argument to `write_to_yaml` and `load_from_yaml`). Hence no struct is allowed have a field with this name.
* This isn't meant to be fast or efficient.

There is overlap with the functionality in StructTypes, and that package is more mature than this with far more support in the package ecosystem. However, it's simpler to make an arbitrary struct work with this package (generally, the user need not do anything at all) than with StructTypes, even for fields with abstract types.

YAML and JSON support are provided by package extensions. This means `PortableStructs` itself does not depend directly on either parser. Load `YAML` before calling `load_from_yaml` or `write_to_yaml`, and load `JSON` before calling `load_from_json` or `write_to_json`.

## Extension points

PortableStructs has two main customization hooks.

`PortableStructs.to_dict(v; type_key, kwargs...)` converts a Julia value into the plain Julia data that a file-format extension can write: scalars, vectors, and dictionaries with string keys. Extend this when a type should have a more compact or semantic representation than "all fields plus a type tag". For example, a data-backed object might write only a filename.

`PortableStructs.from_dict(::Type{T}, value; type_key, base_module, kwargs...)` converts parsed data back into `T`. Extend this for types you own when the generic keyword-constructor path is not right. For types owned by other packages, prefer a small adapter package or package extension rather than adding broad methods in reusable libraries.

The parser-specific dictionary functions live at the file-format boundary:

* `PortableStructs.load_yaml_dict(filename; include_key = "include")`
* `PortableStructs.write_yaml_dict(filename, dict)`
* `PortableStructs.load_json_dict(filename; include_key = "include")`
* `PortableStructs.write_json_dict(filename, dict; indent = 4)`

These functions are implemented by the YAML and JSON extensions. They are useful if you want to work directly at the dictionary layer, or if you are writing another format extension and want to mirror the same pattern: parse a file into dictionaries, let PortableStructs expand includes and construct values, then write dictionaries back out.

Design notes:

* The key reason this package exists, instead of just using StructTypes, is that this handles abstract types where the potential subtypes of the abstract type aren't known (one can't write a `StructTypes.subtypes` function to resolve which abstract type should be constructed).
