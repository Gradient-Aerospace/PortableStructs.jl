"""
The PortableStructs module provides an easy way to write out structs as YAML/JSON and also
to load YAML/JSON and populate the appropriate struct.

It is easy to write (most) structs-of-structs out to a YAML file:

```
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

Design notes:

* We could potentially put `load_from_yaml` in a YAML extension, `load_from_json` in a JSON
  extension, etc.
* The key reason this package exists, instead of just using StructTypes, is that this
  handles abstract types where the potential subtypes of the abstract type aren't known (one
  can't write a `StructTypes.subtypes` function to resolve which abstract type should be
  constructed).
"""
module PortableStructs

export load_from_yaml, write_to_yaml

using StaticArrays: SVector
using OrderedCollections: OrderedDict
import YAML

# All of these from_dict methods are specifically meant for building types from
# dictionaries, such as might be loaded from YAML or JSON.

# TODO: Support dicts on the RHS where the keys are symbols? Maybe later.

# Loading from YAML will result in the following types on the right-hand side (RHS):
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
from reading the YAML file, so an int, float, string, date, vector, or dict.

The keyword arguments come directly from [`load_from_yaml`](@ref).
"""
function from_dict end

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

# Tuples (that aren't NTuples) involve from_dicting each element from its individual type.
function from_dict(::Type{T}, v::Vector; kwargs...) where {T <: Tuple}
    return Tuple(from_dict(ft, el; kwargs...) for (ft, el) in zip(fieldtypes(T), v))
end

# SVectors are like vectors; from_dict each element individually inside an SVector.
function from_dict(::Type{SVector{N, ET}}, v::Vector; kwargs...) where {N, ET}
    return SVector{N, ET}(from_dict(ET, el; kwargs...) for el in v)
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

# Here's the big one. For constructing general composite types, there are really two
# different cases to handle. If the type we seek is concrete, then we can try to construct
# it with keyword arguments. We'll use each field's type to construct each value of the
# keyword arguments.
#
# But if the type we're constructing is abstract, we need to see which concrete thing to
# construct. We'll look for a "type" field that tells us. However, that will be a string.
# How do we get the corresponding type? We could just evaluate the string as an expression,
# but we're prefer to avoid direct evaluation of expressions here. Instead, we'll see if
# we can find any subtype of the sought type whose string representation matches what's
# coming from the dictionary. This involves using subtypes, which means we depend on
# InteractiveUtils. That's not totally inappropriate for what we're doing, but it feels a
# little funny to rely on that in an otherwise non-interactive use case.
#
function from_dict(::Type{T}, dict::AbstractDict; type_key, base_module) where {T}
    # println("Constructing a $T...")
    if isconcretetype(T)
        # println("This type is concrete, so we can construct it directly.")
        return from_named_tuple(
            T,
            NamedTuple(
                Symbol(k) => from_dict(fieldtype(T, Symbol(k)), v; type_key, base_module)
                for (k, v) in pairs(dict) if k != type_key
            ),
        )
    elseif haskey(dict, type_key)
        type_string = dict[type_key]
        module_name = base_module
        module_path = split(type_string, ".")
        for k in 1:length(module_path)-1
            try
                module_name = getfield(module_name, Symbol(module_path[k]))
            catch err
                error("Could not find the $(module_path[k]) module in the $module_name module.")
            end
        end
        type_symbol = Symbol(last(module_path))
        type = try
            # println("Found $type_string in $module_name")
            getfield(module_name, type_symbol)
        catch err
            error("The $type_symbol type could not be found in $module_name.")
        end
        # return from_dict(type, dict; type_key, base_module)
        for (k, v) in pairs(dict)
            if k != type_key
                Symbol(k) => from_dict(fieldtype(type, Symbol(k)), v; type_key, base_module)
            end
        end
        return from_named_tuple(
            type,
            NamedTuple(
                Symbol(k) => from_dict(fieldtype(type, Symbol(k)), v; type_key, base_module)
                for (k, v) in pairs(dict) if k != type_key
            ),
        )
    end
    error("Could not construct a $T from the given dictionary. Adding a \"$type_key\" key would help resolve which type to construct.")
end

"""
    load_from_yaml(filename [, t::Type]; kwargs...)

Loads the given filename and constructs type `t` (if given).

Keyword arguments:

* `type_key`: Determines what field in the YAML is used to say which type should be used
  in construction. Default: "type".
* `base_module`: The module to search for types called out in the YAML file. Default: Main
"""
function load_from_yaml(filename::AbstractString, t::Type; type_key = "type", base_module = Main)
    dict = YAML.load_file(filename; dicttype = OrderedDict{String, Any})
    return from_dict(t, dict; type_key, base_module)
end

load_from_yaml(filename::AbstractString; kwargs...) = load_from_yaml(filename, Any; kwargs...)

"""
    to_dict(v; kwargs...)

Builds a "dictionary value" (something that can be written to YAML natively) for the given
value.

The keyword arguments come directly from [`write_to_yaml`](@ref).
"""
function to_dict end

to_dict(v::Unsigned; kwargs...) = string(v) # Numbers load as Int64 which can't store a UInt64, so we store and load unsigned numbers as strings.
to_dict(v::Union{Integer, AbstractFloat, AbstractIrrational}; kwargs...) = v
to_dict(v::AbstractString; kwargs...) = v
to_dict(v::AbstractChar; kwargs...) = v
to_dict(v::Enum; kwargs...) = string(v)
to_dict(v::AbstractVector; kwargs...) = [to_dict(el; kwargs...) for el in v]
to_dict(v::Tuple; kwargs...) = [to_dict(el; kwargs...) for el in v]
to_dict(v::NamedTuple; kwargs...) = OrderedDict(string(k) => to_dict(el; kwargs...) for (k, el) in pairs(v))
to_dict(v::AbstractDict; kwargs...) = OrderedDict(string(k) => to_dict(el; kwargs...) for (k, el) in pairs(v))
function to_dict(v; type_key)
    m = match(r"^(\w+\.)*(\w+)", string(typeof(v)))
    type_string = m.match
    dict = OrderedDict{String, Any}(type_key => type_string)
    for fn in fieldnames(typeof(v))
        dict[string(fn)] = to_dict(getfield(v, fn); type_key)
    end
    return dict
end

"""
    write_to_yaml(filename, v; kwargs...)

Creates `filename` as a YAML file and populates it with the contents of the given value.

Keyword arguments:

* `type_key`: Determines what field in the YAML is used to say which type should be used
  in construction. Default: "type".
"""
function write_to_yaml(filename::AbstractString, v; type_key = "type")
    dict = to_dict(v; type_key)
    YAML.write_file(filename, dict)
    return nothing
end

end # module PortableStructs
