"""
The PortableStructs module provides an easy way to write out structs as YAML/JSON and also
to load YAML/JSON and populate the appropriate struct.

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

This package is meant to be simple, and that simplicity comes from several constraints:

* The user's structs will be constructed entirely from keyword arguments, one for each
  field, so they must have constructors that support this (such as by adding `@kwdef` in
  front of the struct definition).
* The type of each struct will show up in the YAML file with a key called "type" (or
  whatever string is specified by the `type_key` keyword argument to `write_to_yaml` and
  `load_from_yaml`). Hence no struct is allowed have a field with this name.
* This isn't meant to be fast or efficient.

There is overlap with the functionality in StructTypes. This package is not as flexible as
that one, but it's simpler to make an arbitrary struct work with this package (generally,
the user need not do anything at all!) than with StructTypes.

YAML and JSON support are provided by package extensions. Load `YAML` before calling
`load_from_yaml` or `write_to_yaml`, and load `JSON` before calling `load_from_json` or
`write_to_json`.

Design notes:

* The key reason this package exists, instead of just using StructTypes, is that this
  handles abstract types where the potential subtypes of the abstract type aren't known (one
  can't write a `StructTypes.subtypes` function to resolve which abstract type should be
  constructed).
"""
module PortableStructs

export load_from_yaml, write_to_yaml, load_from_json, write_to_json

using StaticArrays: SVector
using OrderedCollections: OrderedDict

# All of these from_dict methods are specifically meant for building types from
# dictionaries, such as might be loaded from YAML or JSON.

# TODO: Support dicts on the RHS where the keys are symbols? Maybe later.

# Loading from a data format will result in the following types on the right-hand side
# (RHS), depending on the parser used by the format extension:
#
# * Int64   -> Convert to the desired number type.
# * Float64 -> Convert to the desired number type.
# * String  -> Convert to the string, enum, char, rational, or complex.
# * Date    -> These aren't handled right now. (Dates should be strings and use quotes.)
# * Vector  -> Convert each element to the eltype of the left-hand side and construct the vector, ntuple, or tuple.
# * Dict    -> Convert each element to the eltype of the LHS and construct the type, named tuple, or dict.
#

"""
    from_dict(t::Type, v; kwargs...)

Constructs the given type from the given value. The type of the value will be whatever comes
from reading the source file, so an int, float, string, date, vector, or dict.

The keyword arguments come directly from the format-specific load functions, such as
[`load_from_yaml`](@ref) or [`load_from_json`](@ref).
"""
function from_dict end

# The decoding path is intentionally split into a few small stages:
#
# 1. YAML/JSON gives us ordinary Julia containers and scalars.
# 2. If a mapping has a type tag, we resolve that string to a Type or Function.
# 3. We recursively convert each raw child value using the type expected by its field.
# 4. We construct the final object from those converted children.
#
# Keeping those stages separate makes the broad generic fallback easier to reason about.
# It also gives future work obvious places to plug in policy: a safer type resolver, a
# registry of user-provided constructors, or stricter conversion rules can each live in the
# appropriate stage without turning `from_dict` into one large conditional.

# If the value is already the sought-after type, just use it.
function from_dict(t::Type{T}, v::V; kwargs...) where {T, V <: T}
    return v
end

# If they're both numbers but not the same type, convert them explicitly.
function from_dict(::Type{T}, v::AbstractFloat; kwargs...) where {T <: AbstractFloat}
    return convert(T, v)
end
function from_dict(::Type{T}, v::Int, kwargs...) where {T <: Int}
    return convert(T, v)
end

# Rationals like "1//2" *can* load as strings, but we can parse them as rationals.
function from_dict(::Type{T}, v::String; kwargs...) where {T <: Rational}
    return parse(T, v)
end

# Complex numbers like "1+2im" *can* load as strings, but we can parse them.
function from_dict(::Type{T}, v::String; kwargs...) where {T <: Complex}
    return parse(T, v)
end

# Chars can come from strings.
function from_dict(t::Type{<:AbstractChar}, v::String; kwargs...)
    return only(v)
end

# Enums can come from strings.
function from_dict(t::Type{<:Enum}, v::String; kwargs...)
    for i in instances(t)
        if string(i) == v
            return i
        end
    end
    error("\"$v\" did not map to any enum of type $t.")
end

# Not all strings can become symbols, but we'll give it a try.
function from_dict(t::Type{Symbol}, v::String; kwargs...)
    return Symbol(v)
end

# If we want some other type, and we have a string, try to parse as that type.
function from_dict(t::Type{T}, v::String; kwargs...) where {T}
    if String <: T
        return v # I'm not sure why dispatch doesn't already do this.
    else
        return parse(t, v)
    end
end

# If we need a dict with string keys, well, that's what the RHS is already, right? But we
# still need to dive in and attempt to from_dict each element.
function from_dict(::Type{T}, v::AbstractDict; kwargs...) where {T <: AbstractDict{String, VT}} where {VT}
    return T(key => from_dict(VT, el; kwargs...) for (key, el) in pairs(v))
end

# # If we're loading up a vector, from_dict each element individually.
function from_dict(t::Type{<:Vector}, v::Vector; kwargs...)
    return [from_dict(eltype(t), el; kwargs...) for el in v]
end

# NTuples are like vectors; from_dict each element individually inside a tuple.
function from_dict(::Type{NTuple{N, ET}}, v::Vector; kwargs...) where {N, ET}
    return Tuple(from_dict(ET, el; kwargs...) for el in v)
end
function from_dict(t::Type{<:NTuple}, v::Vector; kwargs...) # Length is unknown
    els = [from_dict(eltype(t), el; kwargs...) for el in v]
    return NTuple{length(els), eltype(t)}(els)
end

# Tuples (that aren't NTuples) involve from_dicting each element from its individual type.
function from_dict(::Type{T}, v::Vector; kwargs...) where {T <: Tuple}
    return Tuple(from_dict(ft, el; kwargs...) for (ft, el) in zip(fieldtypes(T), v))
end

# SVectors are like vectors; from_dict each element individually inside an SVector.
function from_dict(::Type{SVector{N, ET}}, v::Vector; kwargs...) where {N, ET}
    return SVector{N, ET}(from_dict(ET, el; kwargs...) for el in v)
end
function from_dict(t::Type{<:SVector}, v::Vector; kwargs...) # Length is unknown
    els = [from_dict(eltype(t), el; kwargs...) for el in v]
    return SVector{length(els), eltype(t)}(els)
end

# TODO: Move this to an extension.
# Xoshiro has no keyword constructor, so we make a from_dict method that constructs a
# Xoshiro directly. Note that this pattern only assumes that the keys are in the right order
# (and they should be since we use OrderedDicts), that the fields can be constructed
# using precisely the types given in fieldtypes, and that the overall type (Xoshiro) can be
# constructed from its individual fields. That makes this a pattern we could probably use
# with a lot of different types. Maybe we could even see if methodexists for the keyword
# argument constructor and, if it doesn't, see if method exists for the below.
using Random: Xoshiro
function from_dict(::Type{Xoshiro}, v::AbstractDict{<:AbstractString, <:Any}; kwargs...)
    return Xoshiro(
        (
            from_dict(ft, v[string(fn)]; kwargs...)
            for (ft, fn) in zip(fieldtypes(Xoshiro), fieldnames(Xoshiro))
        )...,
    )
end
function from_dict(type::Type{<:Unsigned}, v::String; kwargs...)
    return parse(type, v)
end

# If we seek a fully characterized named tuple but have a dict, let the keys be the names.
function from_dict(::Type{NamedTuple{F, T}}, v::AbstractDict{<:AbstractString, <:Any}; kwargs...) where {F, T}
    return NamedTuple(
        fn => from_dict(ft, v[string(fn)]; kwargs...)
        for (fn, ft) in zip(F, fieldtypes(T))
    )
end

# If we seek a generic named tuple but have a dict, let the keys be the names.
function from_dict(::Type{NamedTuple}, v::AbstractDict{<:AbstractString, <:Any}; kwargs...)
    return NamedTuple(
        Symbol(key) => from_dict(Any, el; kwargs...)
        for (key, el) in pairs(v)
    )
end

# If we are trying to load something as a function, see if it has a proper name we can use.
function from_dict(::Type{<:Function}, v::String; base_module, kwargs...)

    # Anonymous functions can't be loaded. They'll look like var"#1#2"(). Give a helpful
    # error.
    if startswith(v, "var\"#")
        error("Could not load an anonymous function identified as $v.")
    end

    f = resolve_name(v; base_module)

    # If that's not a function, then we aren't delivering what was requested. Bail out.
    if !isa(f, Function)
        error("Could not load the given function: $v")
    end

    return f

end

# If the eltype of the dict is known, we can use that.
function from_dict(t::Type{<:AbstractDict{<:AbstractString, T}}, v::AbstractDict; kwargs...) where {T}
    return OrderedDict( # We can use any type that's an AbstractDict.
        key => from_dict(T, el; kwargs...)
        for (key, el) in pairs(v)
    )
end

# If the eltype of the dict isn't known...
function from_dict(t::Type{<:AbstractDict}, v::AbstractDict; kwargs...)
    return OrderedDict( # We can use any type that's an AbstractDict.
        key => from_dict(Any, el; kwargs...)
        for (key, el) in pairs(v)
    )
end

"""
    from_named_tuple(type::Type, named_tuple)

Constructs the given type from arguments in the named tuple, where the keys are field names
and the values are the field values.

By default, this simply calls `type(; named_tuple...)` -- constructing the type via keyword
arguments.
"""
from_named_tuple(::Type{T}, named_tuple::NamedTuple) where {T} = T(; named_tuple...)
from_named_tuple(::Type{T}, nt::NamedTuple) where {T <: Rational} = T(nt.num, nt.den)
from_named_tuple(::Type{T}, nt::NamedTuple) where {T <: Complex} = T(nt.re, nt.im)
from_named_tuple(f::Function, nt::NamedTuple) = f(; nt...) # This _runs_ a function.

"""
    resolve_name(name; base_module)

Resolve a dotted name like `"A.B.C"` by starting at `base_module` and walking through the
named modules/bindings.

This is deliberately a tiny, isolated piece of the loader because it is the part with the
strongest policy implications. Today, PortableStructs trusts the file and resolves names
from `base_module`. A future public API could replace or wrap this function with an
allow-list or registry without changing how recursive conversion or construction works.
"""
function resolve_name(name::AbstractString; base_module)
    module_name = base_module
    module_path = split(name, ".")
    for k in 1:length(module_path)-1
        try
            module_name = getfield(module_name, Symbol(module_path[k]))
        catch err
            error("Could not find the $(module_path[k]) module in the $module_name module.")
        end
    end

    binding_symbol = Symbol(last(module_path))
    return try
        getfield(module_name, binding_symbol)
    catch err
        error("The $binding_symbol type/function could not be found in $module_name.")
    end
end

function resolve_name(name; base_module)
    error("Type/function tags must be strings, but got a $(typeof(name)).")
end

function resolve_constructor_tag(dict; type_key, base_module)
    target = resolve_name(dict[type_key]; base_module)
    if target isa Type || target isa Function
        return target
    end
    error("The \"$(dict[type_key])\" tag resolved to $target, which is not a type or function.")
end

# This is the recursive conversion stage for composite values. For a concrete type, every
# key in the dictionary corresponds to a field, and the field type tells us how to decode
# that raw parsed value. This is what lets a vector field apply conversion to each
# element, or an abstract field receive a tagged concrete value.
function constructor_arguments(type::Type, dict; type_key, base_module)
    return NamedTuple(
        Symbol(k) => from_dict(fieldtype(type, Symbol(k)), v; type_key, base_module)
        for (k, v) in pairs(dict) if k != type_key
    )
end

# When the tag names a function, we do not have field annotations to guide conversion, so
# children are decoded as `Any`. This preserves current behavior, but it is also the part
# users should treat as trusted-input-only: constructing via a function means running code.
function constructor_arguments(::Function, dict; type_key, base_module)
    return NamedTuple(
        Symbol(k) => from_dict(Any, v; type_key, base_module)
        for (k, v) in pairs(dict) if k != type_key
    )
end

function construct_from_mapping(target::Union{Type, Function}, dict; type_key, base_module)
    return from_named_tuple(
        target,
        constructor_arguments(target, dict; type_key, base_module),
    )
end

# Once a tagged value has been constructed, make sure it fits the type requested by the
# caller or by the containing field. The explicit `convert` keeps useful Julia conversions
# available without mixing that concern into name resolution or field walking.
function finish_decoded_value(::Type{T}, value) where {T}
    if value isa T
        return value
    else
        return convert(T, value)
    end
end

# This is for constructing general composite types. If there's a type key, we'll use that
# and attempt to instantiate that type via keyword arguments. If there's no type key, but
# the input type is concrete (so, we know the types of the fields), we will try to construct
# that via keyword arguments, using the appropriate type for each field.
function from_dict(::Type{T}, dict::AbstractDict; type_key, base_module) where {T}
    # println("Constructing a $T...")
    if haskey(dict, type_key)
        target = resolve_constructor_tag(dict; type_key, base_module)
        value = construct_from_mapping(target, dict; type_key, base_module)
        return finish_decoded_value(T, value)
    elseif isconcretetype(T)
        # println("This type is concrete, so we can construct it directly.")
        return construct_from_mapping(T, dict; type_key, base_module)
    elseif dict isa T
        # Note: This isn't a great fallback. We should keep calling from_dict on the
        # children, but without a concrete type, we don't know how to instantiate it. So,
        # this does nothing recursive. If the dict is already what was requested, then
        # that's what you'll get.
        return dict
    end
    error("Could not construct a $T from the given dictionary:\n\n$(dict)\n\n Adding a \"$type_key\" key would help resolve which type to construct.")
end

function make_exception!(d, path, value)

    # Try to match from the beginning of a string to the first dot, capturing everything
    # before and everything after the dot.
    m = match(r"^([^.]+)\.(.+)$", path)

    # If there were no matches, assume this is a new valud to add.
    if isnothing(m)

        @assert haskey(d, path) "\"$path\" is not a valid key. Available keys: $(keys(d))."
        d[path] = value

    else

        @assert haskey(d, m.captures[1]) "While overwriting the value in \"$path\", the \"$(m.captures[1])\" key was not found. Available keys: $(keys(d))."
        make_exception!(d[m.captures[1]], m.captures[2], value)

    end

end

function fetch_included_file(d, dir, include::AbstractDict, load_dict; include_key)

    @assert haskey(include, "source") "No source was provided for an include entry."

    # See if we should use the given file name (absolute path) or join it with our
    # current path. Also, remove that key.
    filename = if isabspath(include["source"])
        include["source"]
    else
        joinpath(dir, include["source"])
    end
    delete!(d, include_key)

    # Now do exactly the same thing that was done to get here in the first place, using
    # the parser supplied by the active format extension.
    subdict = load_dict(filename)
    subdict = expand_include_files(subdict, dirname(filename), load_dict; include_key)

    # Let any other keys in the dictionary overwrite what we loaded (the parent is
    # allowed to overwrite the child).
    d = merge(subdict, d)

    # Now process the "except"s.
    if haskey(include, "except")
        for exception in include["except"]
            make_exception!(d, exception["path"], exception["value"])
        end
    end

    return d

end

function fetch_included_file(d, dir, include::AbstractString, load_dict; include_key)
    include = Dict(
        "source" => include,
    )
    return fetch_included_file(d, dir, include, load_dict; include_key)
end

# Replace "include" with a dictionary loaded from the given file name.
function expand_include_files(d, dir, load_dict; include_key = "include")

    # First, do this recursively on all keys that are dictionaries.
    for k in keys(d)
        if k != include_key && d[k] isa AbstractDict
            d[k] = expand_include_files(d[k], dir, load_dict; include_key)
        end
    end

    # Now if there's an "include" in there, load that file, and expand it the same way we
    # were expanded.
    if haskey(d, include_key)
        d = fetch_included_file(d, dir, d[include_key], load_dict; include_key)
    end

    # Return the possibly updated, possibly completely replaced dictionary.
    return d

end

"""
    load_from_yaml(filename [, t::Type]; kwargs...)

Loads the given filename and constructs type `t` (if given).

This method is provided by the YAML extension. Load `YAML` in the active environment before
calling it.

Keyword arguments:

* `type_key`: Determines what field in the YAML is used to say which type should be used
  in construction. Default: "type".
* `base_module`: The module to search for types called out in the YAML file. Default: Main
* `include_key`: Determines what field in the YAML is used to include another YAML file.
  Default: "include".
"""
function load_from_yaml end

"""
    write_to_yaml(filename, v; kwargs...)

Creates `filename` as a YAML file and populates it with the contents of the given value.

This method is provided by the YAML extension. Load `YAML` in the active environment before
calling it.

Keyword arguments:

* `type_key`: Determines what field in the YAML is used to say which type should be used
  in construction. Default: "type".
"""
function write_to_yaml end

"""
    load_from_json(filename [, t::Type]; kwargs...)

Loads the given filename and constructs type `t` (if given).

This method is provided by the JSON extension. Load `JSON` in the active environment before
calling it.

Keyword arguments:

* `type_key`: Determines what field in the JSON is used to say which type should be used
  in construction. Default: "type".
* `base_module`: The module to search for types called out in the JSON file. Default: Main
* `include_key`: Determines what field in the JSON is used to include another JSON file.
  Default: "include".
"""
function load_from_json end

"""
    write_to_json(filename, v; kwargs...)

Creates `filename` as a JSON file and populates it with the contents of the given value.

This method is provided by the JSON extension. Load `JSON` in the active environment before
calling it.

Keyword arguments:

* `type_key`: Determines what field in the JSON is used to say which type should be used
  in construction. Default: "type".
* `indent`: Determines the number of spaces used when pretty-printing JSON. Default: 4.
"""
function write_to_json end

# These generics are implemented by format extensions. They keep parser-specific behavior
# at the edge: the core package only knows how to turn dictionaries into structs and structs
# into dictionaries.
function load_yaml_dict end
function write_yaml_dict end
function load_json_dict end
function write_json_dict end

"""
    to_dict(v; kwargs...)

Builds a "dictionary value" (something that can be written by a format extension) for the
given value.

The keyword arguments come directly from the format-specific write functions, such as
[`write_to_yaml`](@ref) or [`write_to_json`](@ref).
"""
function to_dict end

to_dict(v::Unsigned; kwargs...) = string(v) # Numbers load as Int64 which can't store a UInt64, so we store and load unsigned numbers as strings.
to_dict(v::Union{Integer, AbstractFloat, AbstractIrrational}; kwargs...) = v
to_dict(v::AbstractString; kwargs...) = v
to_dict(v::AbstractChar; kwargs...) = v
to_dict(v::Symbol; kwargs...) = string(v)
to_dict(v::Enum; kwargs...) = string(v)
to_dict(v::Function; kwargs...) = repr(v)
to_dict(v::AbstractVector; kwargs...) = [to_dict(el; kwargs...) for el in v]
to_dict(v::Tuple; kwargs...) = [to_dict(el; kwargs...) for el in v]
to_dict(v::NamedTuple; kwargs...) = OrderedDict(string(k) => to_dict(el; kwargs...) for (k, el) in pairs(v))
to_dict(v::AbstractDict; kwargs...) = OrderedDict(string(k) => to_dict(el; kwargs...) for (k, el) in pairs(v))

function type_tag(v)
    # Try to figure out the type. This will search for Module.Submodule.Type. Any type
    # parameters will be dropped. Dropping parameters is intentional here: when loading,
    # field annotations and keyword constructors usually reconstruct concrete parameters
    # from the child values. Keeping this in one helper makes that policy easy to revisit.
    m = match(r"^(\w+\.)*(\w+)", string(typeof(v)))
    if isnothing(m)
        error("The string, $v, could not be interpreted as a type.")
    end
    return m.match
end

# The generic write path mirrors the generic load path: emit a type tag, then emit one
# recursively encoded entry per field. Specialized `to_dict` methods can replace this for
# compact or semantic representations, such as storing a filename instead of a large payload.
function to_dict(v; type_key)
    dict = OrderedDict{String, Any}(type_key => type_tag(v))
    for fn in fieldnames(typeof(v))
        dict[string(fn)] = to_dict(getfield(v, fn); type_key)
    end

    return dict

end

end # module PortableStructs
