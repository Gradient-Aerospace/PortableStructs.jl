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

## YAML Example

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

## JSON Example

The same dictionary representation from above can be written to JSON. The JSON methods are available once `JSON` is loaded:

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

## Includes and Exceptions

A YAML file can be "included" at any level. This allows the user to break up a large YAML file into smaller ones. By default, the key `include` will be used to indicate what file to include. The `include_key` keyword argument to `load_from_yaml` can specify a different key to use (e.g., `_include`). When including files, the file name is assumed to be relative to the file that has the "include" in it (or an absolute path).

Includes can also provide `except` entries to overwrite values from the included file. Exception paths use dot-separated dictionary keys. They can also target existing vector elements with 1-based indices, matching Julia's indexing. For example, `trees[2].common_name` overwrites the `common_name` key in the second element of the `trees` vector. Vector indices in paths must already exist; overwrite exceptions do not append to vectors or create missing vector entries.

For example, `trees.yaml` might provide shared data:

```yaml
trees:
  - scientific_name: Arbutus unedo
    common_name: strawberry tree
  - scientific_name: Arbutus menziesii
    common_name: madrona
notes: Needs review.
```

Another file can include it and overwrite selected values:

```yaml
include:
  source: trees.yaml
  except:
    - path: trees[2].common_name
      value: Pacific madrone
    - path: notes
      value: Reviewed.
```

Loading the second file produces the included data with the second tree's common name changed and the top-level `notes` value replaced. The `trees[2]` path uses 1-based indexing to be consistent with Julia.

### Adding, Deleting, and Merging Included Data

Each `except` entry can specify an `operation`. The default is `overwrite`. A `path` selects an existing value using the same dictionary keys and 1-based vector indices shown above. Omitting `path` selects the entire included value.

The operations accept the following operands:

| Operation | Operand | Behavior |
| --- | --- | --- |
| `overwrite` (default) | `value` | Replaces the selected value. |
| `merge` | `value` | Merges a dictionary into the selected dictionary. Incoming values overwrite matching keys, and new keys are added. |
| `add` | `values` | Adds entries to a dictionary, or appends elements to a vector. Dictionary keys must all be absent. |
| `delete` | `keys` | Removes the listed keys from a dictionary. All keys must exist. |
| `delete` | `indices` | Removes the listed 1-based indices from a vector. All indices must exist. |

For `add`, `values` is a dictionary when adding dictionary entries and a list when appending vector elements. Vector additions preserve values as though the combined sequence had been written directly in the YAML or JSON file. For `delete`, `keys` and `indices` are lists. Repeated keys or indices are harmless: the selected entries are removed once. Each add or delete checks all requested keys or indices before changing the target.

For example, suppose `baseline.yaml` contains:

```yaml
settings:
  gain: 1.0
  obsolete_option: true
  controller:
    gain: 0.5
    limit: 10
vehicles: [alpha, bravo, charlie, delta, echo]
```

We can merge settings, add several new options, and delete an obsolete option in one including file:

```yaml
include:
  source: baseline.yaml
  except:
    - path: settings
      operation: merge
      value:
        gain: 2.0
        controller: {gain: 0.8}
    - path: settings
      operation: add
      values: {enabled: true, mode: cruise}
    - path: settings
      operation: delete
      keys: [obsolete_option]
```

The resulting `settings` contains `gain: 2.0`, `controller: {gain: 0.8}`, `enabled: true`, and `mode: cruise`. Merge is shallow: the incoming `controller` dictionary replaces the previous one, so its old `limit` entry is discarded. Existing dictionary keys keep their positions, and new keys are appended in operand order.

Vector deletions happen together, using the indices before that exception runs. We can remove the first and third vehicles, then append two more:

```yaml
include:
  source: baseline.yaml
  except:
    - path: vehicles
      operation: delete
      indices: [1, 3]
    - path: vehicles
      operation: add
      values: [foxtrot, golf]
```

The resulting `vehicles` is `[bravo, delta, echo, foxtrot, golf]`. Exceptions run in their listed order, so a later exception sees the changes made by earlier exceptions. Sibling entries alongside `include` are applied before any exceptions.

An operand can itself include a file. To merge two complete dictionaries, we can omit `path` and include the second file as the merge value:

```yaml
include:
  source: baseline.yaml
  except:
    - operation: merge
      value:
        include: overrides.yaml
```

Every top-level key in `overrides.yaml` is added to or overwrites the corresponding key from `baseline.yaml`. Relative filenames inside exception operands are resolved relative to the file containing the exception, even when the source file is in another directory.

Nested includes also let us replace a value with a separate baseline and then modify that baseline:

```yaml
include:
  source: baseline.yaml
  except:
    - path: settings
      value:
        include:
          source: controller_settings.yaml
          except:
            - operation: merge
              value: {gain: 3.0}
```

Here `settings` is replaced by the contents of `controller_settings.yaml` with `gain` set to `3.0`. The outer exception uses the default `overwrite` operation. The inner merge selects the root of the included settings because it has no `path`.

The same operations are available in JSON includes. When using a custom `include_key`, nested includes use that key too; the exception fields such as `operation` and `values` retain their names.

Exception entries accept only `path`, `operation`, and the operand fields listed above, so misspelled fields produce an error. Invalid include specifications, exception operations, and target paths raise `PortableStructs.IncludeError`, which reports the files being loaded and the location of the include within each file. Exception errors also identify the exception number, operation, and target path (`<root>` when the path is omitted). For nested includes, the error retains the chain of files and operations that led to the failure. These locations refer to dictionary keys and vector indices, rather than source line numbers. Parser, file-read, and unexpected errors propagate unchanged with their original exception types and backtraces.

## Notes on Specific Types

### Regular expressions

A field declared as `Regex` accepts a plain pattern string, such as
`expression: 'apple$'`. Saving it produces a tagged representation that can also be
loaded into an `Any` field:

```yaml
expression:
  type: Regex
  pattern: 'apple$'
```

Non-default `compile_options` and `match_options` are saved as decimal strings and
restored through Julia's regex constructor. This preserves options such as
case-insensitive matching when saving and loading an expression.
The same representation is used for JSON. Invalid patterns raise an error when loaded.

### Tuples in Untyped Fields

YAML and JSON sequences normally load as vectors when the containing field does not provide
a more specific type. A dictionary can explicitly identify a tuple and provide its values
under `args`:

```yaml
size:
  type: Tuple
  args: [1024, 768]
```

Each entry under `args` is recursively decoded, so it can also contain tagged values. This
representation is intended for hand-authored inputs; ordinary tuple serialization remains a
plain sequence.

## Type Names and `base_module`

PortableStructs writes type names and resolves type names relative to a `base_module`. By
default, `base_module = Main`, which is a good match for scripts and REPL work where the
types being loaded are available from `Main`.

Package code that writes its own artifacts can choose the package module as the base module
instead, and they should use the same `base_module` when loading.

Values can also use types from outside the chosen `base_module`. If the base module imports a type under its own name, PortableStructs can write that unqualified name. Otherwise, it writes the type's defining module path, such as `Dates.Date`. When reading and resolving a type name like "A.B.C", this is the order of operations:

* If the type name starts with "Main.", "Base.", or "Core.", resolution will continue from those top-level modules, regardless of the value used for `base_module`.
* Otherwise, if the first word in the type name is available in the `base_module`, the full type will be resolved in that module.
* Otherwise, the type will be resolved in `Main`.
* Otherwise, the loader will throw an error.

A file written with a given `base_module` and a given set of imported modules in `Main` should be loaded with the same `base_module` and the same imports available.

Here is an example of package code writing and reading consistently:

```
module MyPackage

using PortableStructs

@kwdef struct Config
    gain::Float64
end

function save_config(file, config)
    PortableStructs.write_to_yaml(file, config; base_module = @__MODULE__)
    return nothing
end

function load_config(file)
    return PortableStructs.load_from_yaml(file; base_module = @__MODULE__)
end

end
```

With `base_module = @__MODULE__`, the YAML can use a package-relative type tag:

```yaml
type: Config
gain: 1.0
```

Without that explicit `base_module`, the default `Main`-relative representation would need
to name a type that can be resolved from `Main`.

For example, `Dates.Date` can round-trip through another base module as long as `Dates` is
imported when both writing and loading:

```
import Dates
import PortableStructs
import YAML

module C
end

x = Dates.Date(2026, 6, 24)

PortableStructs.write_to_yaml("date.yaml", x; base_module = C)
PortableStructs.load_from_yaml("date.yaml"; base_module = C)
```

## Constraints

This package is meant to be simple, and that simplicity comes from several constraints:

* The user's structs will be constructed either from keyword arguments or from positional arguments. For positional arguments, the YAML/JSON file should have a key matching each field name, and the arguments will be provided to the constructor in the order of the field names (*not* in the order in which they're encountered in the YAML/JSON file).
* The type of each struct will show up in the YAML/JSON file with a key called "type" (or whatever string is specified by the `type_key` keyword argument to `write_to_yaml` and `load_from_yaml`). Hence no struct is allowed to have a field with this name.
* This isn't meant to be fast or efficient.

There is overlap with the functionality in StructTypes, and that package is more mature than this with far more support in the package ecosystem. However, it's simpler to make an arbitrary struct work with this package (generally, the user need not do anything at all) than with StructTypes, even for fields with abstract types.

YAML and JSON support are provided by package extensions. This means `PortableStructs` itself does not depend directly on either parser. Load `YAML` before calling `load_from_yaml` or `write_to_yaml`, and load `JSON` before calling `load_from_json` or `write_to_json`.

## Extension Points

PortableStructs has two main customization hooks.

`PortableStructs.to_dict(v; type_key, kwargs...)` converts a Julia value into the plain Julia data that a file-format extension can write: scalars, vectors, and dictionaries with string keys. Extend this when a type should have a more compact or semantic representation than "all fields plus a type tag". For example, a data-backed object might write only a filename.

`PortableStructs.from_dict(::Type{T}, value; type_key, base_module, kwargs...)` converts
parsed data back into `T`. Extend this for types you own when the generic construction path
is not right. For types owned by other packages, prefer a small adapter package or package
extension rather than adding broad methods in reusable libraries.

The parser-specific dictionary functions live at the file-format boundary:

* `PortableStructs.load_yaml_dict(filename; include_key = "include")`
* `PortableStructs.write_yaml_dict(filename, dict)`
* `PortableStructs.load_json_dict(filename; include_key = "include")`
* `PortableStructs.write_json_dict(filename, dict; indent = 4)`

These functions are implemented by the YAML and JSON extensions. They are useful if you want to work directly at the dictionary layer, or if you are writing another format extension and want to mirror the same pattern: parse a file into dictionaries, let PortableStructs expand includes and construct values, then write dictionaries back out.

## Design Notes

* The key reason this package exists, instead of just using StructTypes, is that this handles abstract types where the potential subtypes of the abstract type aren't known (one can't write a `StructTypes.subtypes` function to resolve which abstract type should be constructed).
