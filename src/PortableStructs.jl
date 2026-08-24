"""
The PortableStructs module provides an easy way to write out structs as YAML/JSON and also
to load YAML/JSON and populate the appropriate struct.

PortableStructs is intended for trusted configuration and data files. Loading a typed file
resolves type/function names from Julia modules and can call constructors or functions, so
it should not be used as a safe deserializer for untrusted input.

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

* The user's structs will be constructed from keyword arguments when possible, or from
  positional arguments when the input keys exactly match the type's field names.
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
# * Vector  -> Convert each element to the eltype of the left-hand side and construct the
#              vector, ntuple, or tuple.
# * Dict    -> Convert each element to the eltype of the LHS and construct the type, matrix,
#              named tuple, or dict.
#

#########################
# Specialized from_dict #
#########################

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

# General from_dict behavior

# If the value is already the sought-after type, just use it.
function from_dict(t::Type{T}, v::V; kwargs...) where {T, V <: T}
    return v
end

# If the type is actually a function, load all children as keyword arguments and then run
# the function.
function from_dict(f::Function, v; kwargs...)
    children = constructor_arguments(f, v; kwargs...)
    return f(; children...)
end

# Numbers

# YAML and JSON distinguish integer and floating-point literals, but the requested Julia
# type determines the representation we ultimately want. Julia's `convert` also preserves
# its usual range and exactness checks for conversions such as Float64 to Int8.
function from_dict(::Type{T}, v::Real; kwargs...) where {T <: Integer}
    return convert(T, v)
end
function from_dict(::Type{T}, v::Real; kwargs...) where {T <: AbstractFloat}
    return convert(T, v)
end

# Rationals like "1//2" *can* load as strings, but we can parse them as rationals.
function from_dict(::Type{T}, v::String; kwargs...) where {T <: Rational}
    return parse(T, v)
end
function from_dict(::Type{Rational{T}}, v::AbstractDict; kwargs...) where {T}
    return Rational{T}(convert(T, v["num"]), convert(T, v["den"]))
end
function from_dict(::Type{Rational}, v::AbstractDict; kwargs...)
    return Rational(v["num"], v["den"])
end

# Complex numbers like "1+2im" *can* load as strings, but we can parse them.
function from_dict(::Type{T}, v::String; kwargs...) where {T <: Complex}
    return parse(T, v)
end
function from_dict(::Type{Complex{T}}, v::AbstractDict; kwargs...) where {T}
    return Complex{T}(convert(T, v["re"]), convert(T, v["im"]))
end
function from_dict(::Type{Complex}, v::AbstractDict; kwargs...)
    return Complex(v["re"], v["im"])
end

function from_dict(type::Type{<:Unsigned}, v::String; kwargs...)
    return parse(type, v)
end

# String-Like

# Chars can come from strings.
function from_dict(::Type{T}, v::String; kwargs...) where {T <: AbstractChar}
    return convert(T, only(v))
end

# Enums can come from strings. Some enum packages, such as EnumX, may be written in scoped
# form by users or external tools. Scoped spellings should resolve to a concrete binding so
# we do not accept a value from the wrong enum just because its final name matches.
function from_dict(t::Type{<:Enum}, v::String; base_module = Main, kwargs...)

    if occursin('.', v)
        value = resolve_name(v; base_module)
        if value isa t
            return value
        end
        error("\"$v\" did not map to any enum of type $t.")
    end

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

# Parse-able

# If we want some other type, and we have a string, try to parse as that type.
function from_dict(t::Type{T}, v::String; kwargs...) where {T}
    if String <: T
        return v # I'm not sure why dispatch doesn't already do this.
    else
        return parse(t, v)
    end
end

# Function

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

# Vectors and tuples

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
    @assert fieldcount(T) == length(v) "Could not construct $T from the fields in the provided value; the following fields were missing: $(setdiff(fieldnames(T), keys(v)))"
    return Tuple(from_dict(ft, el; kwargs...) for (ft, el) in zip(fieldtypes(T), v))
end

# Tuples without known types will build from Any for each element.
function from_dict(::Type{Tuple}, v::Vector; kwargs...)
    return Tuple(from_dict(Any, el; kwargs...) for el in v)
end

# A dictionary representation allows an untyped field to identify itself as a tuple. This
# is useful because YAML and JSON sequences otherwise decode as vectors when the containing
# field does not provide a tuple type annotation.
function from_dict(
    ::Type{Tuple},
    v::AbstractDict;
    type_key,
    base_module,
    kwargs...,
)

    # This method can receive either the original tagged dictionary or the inner dictionary
    # after the generic tagged-value path has removed the type key.
    if haskey(v, type_key)
        target = resolve_constructor_tag(v; type_key, base_module)
        target === Tuple || throw(ArgumentError(
            "Could not construct a Tuple from a \"$(v[type_key])\" type tag.",
        ))
    end

    allowed_keys = haskey(v, type_key) ? (type_key, "args") : ("args",)
    unexpected_keys = setdiff(keys(v), allowed_keys)
    isempty(unexpected_keys) || throw(ArgumentError(
        "A Tuple dictionary may only contain the '$type_key' and 'args' keys, " *
        "but also received $(collect(unexpected_keys)).",
    ))

    haskey(v, "args") || throw(ArgumentError(
        "Could not construct a Tuple because the \"args\" key was missing.",
    ))
    args = v["args"]
    args isa Vector || throw(ArgumentError(
        "Could not construct a Tuple because \"args\" must be a sequence.",
    ))
    return from_dict(Tuple, args; type_key, base_module, kwargs...)

end

# Named Tuples

# If we seek a fully characterized named tuple but have a dict, let the keys be the names.
function from_dict(::Type{NamedTuple{F, T}}, v::AbstractDict{<:AbstractString, <:Any}; kwargs...) where {F, T}
    return NamedTuple(
        fn => from_dict(ft, v[string(fn)]; kwargs...)
        for (fn, ft) in zip(F, fieldtypes(T))
    )
end

# If we seek a generic named tuple but have a dict, let the keys be the names.
function from_dict(::Type{NamedTuple}, v::AbstractDict{<:AbstractString, <:Any}; type_key, kwargs...)
    return NamedTuple(
        Symbol(key) => from_dict(Any, el; type_key, kwargs...)
        for (key, el) in pairs(v) if key != type_key
    )
end


# Dicts

# If the eltype of the dict is known, we can use that.
function from_dict(t::Type{Dict{KT, VT}}, v::AbstractDict; type_key, kwargs...) where {KT <: AbstractString, VT}
    return Dict{KT, VT}(
        KT(key) => from_dict(VT, el; type_key, kwargs...) # Need to make the key have the right type.
        for (key, el) in pairs(v) if key != type_key
    )
end
function from_dict(t::Type{OrderedDict{KT, VT}}, v::AbstractDict; type_key, kwargs...) where {KT <: AbstractString, VT}
    return OrderedDict{KT, VT}(
        KT(key) => from_dict(VT, el; type_key, kwargs...) # Need to make the key have the right type.
        for (key, el) in pairs(v) if key != type_key
    )
end

# If the eltype of the dict isn't known...
function from_dict(t::Type{T}, v::AbstractDict; type_key, kwargs...) where {T <: Union{Dict, OrderedDict}}
    return T( # Infer the key and value types since those weren't provided.
        key => from_dict(Any, el; type_key, kwargs...)
        for (key, el) in pairs(v) if key != type_key
    )
end

# If we're literally asking for the abstraction, then we can return anything satisfying the
# abstraction, so let's choose OrderedDict.
function from_dict(t::Type{AbstractDict}, v::AbstractDict; type_key, kwargs...)
    return OrderedDict(
        key => from_dict(Any, el; type_key, kwargs...)
        for (key, el) in pairs(v) if key != type_key
    )
end
function from_dict(t::Type{AbstractDict{S, T}}, v::AbstractDict; type_key, kwargs...) where {S, T}
    return OrderedDict{S, T}(
        S(key) => from_dict(T, el; type_key, kwargs...)
        for (key, el) in pairs(v) if key != type_key
    )
end

#################################
# Construction of General Types #
#################################

# This returns the module to use for qualifying a module name for a given type, like
# `Main.A.B.MyType` or `Dates.Date`. It returns nothing if it can't figure out where the
# given module is defined.
function root_module_binding(name::AbstractString)

    # See if it's one of the special case modules.
    if name == "Main"
        return Main
    elseif name == "Base"
        return Base
    elseif name == "Core"
        return Core
    end

    # Julia does not expose a public API for looking up an arbitrary loaded root module by
    # name. However, when users write values from a script or REPL, imported packages and
    # stdlibs are normally available as module bindings in Main. That is enough to resolve
    # canonical tags like `Dates.Date` without depending on Base.root_module.
    binding_symbol = Symbol(name)
    if isdefined(Main, binding_symbol)
        binding = getfield(Main, binding_symbol)
        if binding isa Module && nameof(binding) == binding_symbol
            return binding
        end
    end

    return nothing

end

"""
    resolve_name(name; base_module)

Resolve a dotted name like `"A.B.C"` by starting at `base_module` and walking through the
named modules/bindings. If the first name is not available from `base_module`, it is also
allowed to name a root module available from `Main`, such as `Dates`.

This is deliberately a tiny, isolated piece of the loader because it is the part with the
strongest policy implications. Today, PortableStructs trusts the file and resolves names
from `base_module`. A future public API could replace or wrap this function with an
allow-list or registry without changing how recursive conversion or construction works.
"""
function resolve_name(name::AbstractString; base_module)

    module_name = base_module
    module_path = split(name, ".")
    first_symbol = Symbol(first(module_path))

    # Names are usually relative to `base_module`, but serialized values may also carry
    # absolute Julia roots, such as `Main.A.B.x`. We handle these explicitly so absolute
    # names always start from the intended root module, including cases like resolving
    # `Base.x` from a `baremodule`, where `Base` is not bound.
    root_module = root_module_binding(first(module_path))
    first_index = if first(module_path) in ("Main", "Base", "Core")
        module_name = root_module
        2
    elseif !isdefined(base_module, first_symbol) && !isnothing(root_module)
        # A package or stdlib root, such as `Dates`, is not inside `base_module`, but it
        # may be available from Main because the writing/loading script imported it. In
        # that case, treat the first segment as an absolute root and keep resolving the
        # rest of the tag from there.
        module_name = root_module
        2
    else
        1
    end

    if first_index > length(module_path)
        return module_name
    end

    for k in first_index:length(module_path)-1
        next_symbol = Symbol(module_path[k])
        if isdefined(module_name, next_symbol)
            module_name = getfield(module_name, next_symbol)
        else
            error("Could not find the $(module_path[k]) module in the $module_name module.")
        end
    end

    binding_symbol = Symbol(last(module_path))
    if isdefined(module_name, binding_symbol)
        return getfield(module_name, binding_symbol)
    else
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

# This is the recursive conversion stage for composite values. For a type with field
# annotations, each field type tells us how to decode that raw parsed value. This is what
# lets a vector field apply conversion to each element, or an abstract field receive a
# tagged concrete value.
function constructor_arguments(type::Type, dict; type_key, base_module)
    return NamedTuple(
        Symbol(k) => from_dict(
            hasfield(type, Symbol(k)) ? fieldtype(type, Symbol(k)) : Any,
            v;
            type_key,
            base_module,
        )
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

# Dictionary keys normally come from YAML or JSON as strings, but direct `from_dict` calls
# can supply other types whose conversion to `Symbol` may fail.
function possible_field_name(key)
    return try
        Symbol(key)
    catch
        nothing
    end
end

function dictionary_keys_and_field_names(dict; type_key)
    supplied_keys = Tuple(key for key in keys(dict) if key != type_key)
    field_names = map(possible_field_name, supplied_keys)
    return supplied_keys, field_names
end

function has_keyword_constructor(type::Type, dict; type_key)
    _, field_names = dictionary_keys_and_field_names(dict; type_key)
    any(isnothing, field_names) && return false
    return hasmethod(type, Tuple{}, field_names)
end

function keys_matching_fieldnames(type::Type, dict; type_key)

    field_names = try
        fieldnames(type)
    catch
        return nothing
    end

    candidate_keys, candidate_field_names = dictionary_keys_and_field_names(dict; type_key)
    if length(candidate_keys) != length(field_names)
        return nothing
    end
    any(isnothing, candidate_field_names) && return nothing

    keys_by_field = Dict{Symbol, Any}()
    for (key, field_name) in zip(candidate_keys, candidate_field_names)
        if haskey(keys_by_field, field_name)
            return nothing
        end
        keys_by_field[field_name] = key
    end

    if Set(keys(keys_by_field)) != Set(field_names)
        return nothing
    end

    return Tuple(keys_by_field[field_name] for field_name in field_names)

end

function positional_constructor_arguments(type::Type, dict; type_key, base_module)

    field_keys = keys_matching_fieldnames(type, dict; type_key)
    if isnothing(field_keys)
        return nothing
    end

    field_names = fieldnames(type)
    args = Tuple(
        from_dict(fieldtype(type, field_name), dict[key]; type_key, base_module)
        for (field_name, key) in zip(field_names, field_keys)
    )
    arg_types = Tuple{map(typeof, args)...}
    if hasmethod(type, arg_types)
        return args
    end

    error(
        """
        Could not construct a $type from the given dictionary.

        The dictionary keys match the fields $field_names.
        No positional constructor accepts the decoded argument types $arg_types.
        """
    )

end

function dictionary_construction_error_message(type::Type, dict; type_key)

    supplied_keys, supplied_field_names = dictionary_keys_and_field_names(dict; type_key)
    invalid_keys = Tuple(
        key for (key, field_name) in zip(supplied_keys, supplied_field_names)
        if isnothing(field_name)
    )
    valid_field_names = Tuple(
        field_name for field_name in supplied_field_names if !isnothing(field_name)
    )
    lines = [
        "Could not construct a $type from a dictionary.",
        "",
        "Supplied keys: $supplied_keys",
        "No keyword constructor accepts the supplied keys.",
    ]
    !isempty(invalid_keys) && push!(
        lines,
        "Keys that cannot identify Julia fields: $invalid_keys",
    )

    # Positional construction is available only when the input fields match the type's
    # fields exactly. Showing the difference is more useful than printing the input values.
    field_names = try
        fieldnames(type)
    catch
        nothing
    end
    if isnothing(field_names)
        push!(
            lines,
            "Positional construction could not identify a field layout for $type.",
        )
    else
        missing_fields = Tuple(setdiff(field_names, valid_field_names))
        unexpected_keys = Tuple(
            key for (key, field_name) in zip(supplied_keys, supplied_field_names)
            if isnothing(field_name) || field_name ∉ field_names
        )
        push!(
            lines,
            "Positional construction requires the supplied keys to match the fields exactly.",
            "Expected fields: $field_names",
        )
        !isempty(missing_fields) && push!(lines, "Missing fields: $missing_fields")
        !isempty(unexpected_keys) && push!(lines, "Unexpected keys: $unexpected_keys")
    end

    # A type tag is useful when no concrete type can be inferred. It does not help repair
    # a dictionary whose fields simply do not match an already-known concrete type.
    if type === Any || isabstracttype(type)
        push!(
            lines,
            "If this dictionary represents a concrete subtype, identify it with a " *
            "\"$type_key\" key.",
        )
    end

    return join(lines, '\n')

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
# and let the resolved type or function drive construction. If there's no type key, we will
# try keyword construction first, then positional construction when the type's field layout
# lines up exactly with the dictionary.
function from_dict(::Type{T}, dict::AbstractDict; type_key, base_module, kwargs...) where {T}

    if haskey(dict, type_key)

        # There's a key that should tell us what to construct. We'll rely on `from_dict`
        # for this, so that may result in one of the specialized `from_dict` methods. If
        # not, we'll end up back here, but there won't be a type key to use any longer.
        target = resolve_constructor_tag(dict; type_key, base_module)
        new_dict_without_type_key = typeof(dict)(
            key => value for (key, value) in dict if key != type_key
        )
        value = from_dict(target, new_dict_without_type_key; type_key, base_module)
        return finish_decoded_value(T, value)

    elseif OrderedDict <: T

        # If there's no type key, but an OrderedDict already counts as the type we're
        # seeking, then there's no need to try so hard to construct the right type. We can
        # just use an OrderedDict. A great example: we're loading an Any. There's no telling
        # what we should load this as, but we *did* load as a dict already, so we can make
        # an ordered dict of it. We just need to continue processing the children.
        return OrderedDict(
            k => from_dict(Any, v; type_key, base_module)
            for (k, v) in pairs(dict) if k != type_key
        )

    elseif has_keyword_constructor(T, dict; type_key)

        # There's no type key, and this isn't the ordered dict special case, but it appears
        # that there's a keyword constructor for whatever it is we're looking to construct.
        # Let's use that. We'll convert the dict with children to a named tuple and then
        # splat that into the constructor.
        children = constructor_arguments(T, dict; type_key, base_module)
        return T(; children...)

    else

        # If there is no keyword constructor, fall back to positional construction when the
        # dict keys exactly match the field names and the type has a positional constructor
        # that accepts the decoded arguments. Values are decoded in field order so the input
        # mapping order does not affect construction. This also covers parametric types with
        # known field layouts, such as UnitRange.
        args = positional_constructor_arguments(T, dict; type_key, base_module)
        if !isnothing(args)
            return T(args...)
        end

    end

    error(dictionary_construction_error_message(T, dict; type_key))

end

############
# Includes #
############

function first_exception_path_segment(path)

    # Try to match from the beginning of a string to the first dot, capturing everything
    # before and everything after the dot.
    m = match(r"^([^.]+)\.(.+)$", path)

    if isnothing(m)
        return path, nothing
    else
        return m.captures[1], m.captures[2]
    end

end

function key_and_index(segment)

    m = match(r"^([^\[\]]+)\[(\d+)\]$", segment)
    if isnothing(m)
        @assert !occursin('[', segment) && !occursin(']', segment) "\"$segment\" is not a valid exception path segment."
        return segment, nothing
    else
        return m.captures[1], parse(Int, m.captures[2])
    end

end

function check_exception_index(v, index, path)
    @assert index >= 1 "While overwriting the value in \"$path\", a non-positive index was encountered, but indices should be 1-indexed."
    @assert index <= length(v) "While overwriting the value in \"$path\", index $index was not found. Available indices: 1:$(length(v))."
end

function make_exception!(d::AbstractDict, path, value)

    segment, rest = first_exception_path_segment(path)
    key, index = key_and_index(segment)

    # If we're at the end of the path...
    if isnothing(rest)

        # If there were no indices in the path...
        if isnothing(index)

            # Update the element here. The element *must* exist already; adding a new key is
            # likely a typo.
            @assert haskey(d, key) "\"$key\" is not a valid key. Available keys: $(keys(d))."
            d[key] = value

        else

            # There was an index, so we expect to write to an element of a vector.
            @assert haskey(d, key) "While overwriting the value in \"$path\", the \"$key\" key was not found. Available keys: $(keys(d))."
            @assert d[key] isa AbstractVector "While overwriting the value in \"$path\", \"$key\" is not a vector."
            check_exception_index(d[key], index, path)
            d[key][index] = value

        end

    else

        # Otherwise, we're not yet at the end of the path. We'll use recursion to go deeper.
        @assert haskey(d, key) "While overwriting the value in \"$path\", the \"$key\" key was not found. Available keys: $(keys(d))."
        target = d[key]
        if !isnothing(index)
            @assert target isa AbstractVector "While overwriting the value in \"$path\", \"$key\" is not a vector."
            check_exception_index(target, index, path)
            target = target[index]
        end
        make_exception!(target, rest, value)

    end

end

# d is a dictionary, because it had an "include" in it. When we load the included thing and
# have finished searching for includes recursively, the result might be:
#
# * a dictionary, in which case we want to merge the loaded values with d and return that
# * a value, in which case we want to throw away d and return the value
#
function fetch_included_file(d, dir, include::AbstractDict, load_dict; include_key)

    @assert haskey(include, "source") "No source was provided for an include entry."

    # See if we should use the given file name (absolute path) or join it with our
    # current path. Also, remove that key.
    filename = if isabspath(include["source"])
        include["source"]
    else
        joinpath(dir, include["source"])
    end

    # Now do exactly the same thing that was done to get here in the first place, using
    # the parser supplied by the active format extension.
    subdict = load_dict(filename)
    payload = expand_include_files(subdict, dirname(filename), load_dict; include_key)

    # We allow structures like this:
    #
    #   my_value:
    #     include: a_basis_file.yaml
    #     a: "dogs"
    #     b: "cats"
    #
    # This loads `a_basis_file.yaml`, then it overwrites the `a` key and `b` key.
    #
    # We could instead lean on `except` for this, but that leaves open the question of
    # what's to be done if there's an `include` entry *and* other entries. This way is
    # nicely unambiguous.
    #
    # We could use dispatch rather than an `if` for this, but there are limited types we can
    # end up with here, so this seems most direct.
    if payload isa AbstractDict

        # Let any other keys in the dictionary overwrite what we loaded. (The file *doing*
        # the including can overwrite anything that it includes.) Existing keys retain
        # their positions, while new keys are appended in the including file's order.
        merged_payload = OrderedDict(payload)
        for (key, value) in pairs(d)
            if key != include_key
                merged_payload[key] = value
            end
        end
        payload = merged_payload

    end

    # Now process the "except"s. Note that the exceptions may, themselves, have includes, so
    # we'll need to expand those.
    if haskey(include, "except")
        for exception in include["except"]
            expanded_value = expand_include_files(
                exception["value"],
                dir,
                load_dict;
                include_key,
            )
            make_exception!(payload, exception["path"], expanded_value)
        end
    end

    return payload

end

function fetch_included_file(d, dir, include::AbstractString, load_dict; include_key)
    include = Dict(
        "source" => include,
    )
    return fetch_included_file(d, dir, include, load_dict; include_key)
end

# Replace "include" with a dictionary loaded from the given file name.
function expand_include_files(d::AbstractDict, dir, load_dict; include_key = "include")

    # First, do this recursively on all elements
    for k in keys(d)
        if k != include_key
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
function expand_include_files(d::Vector, dir, load_dict; include_key = "include")
    return map(d) do v
        expand_include_files(v, dir, load_dict; include_key)
    end
end
function expand_include_files(d, dir, load_dict; include_key = "include")
    return d
end

##############
# Extensions #
##############

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
* `base_module`: The module from which type and enum value names should be written.
  Default: Main
"""
function write_to_yaml end

function extension_not_loaded_error(package_name, function_name)
    error("`$function_name` requires `$package_name`. Load it with `import $package_name` before calling `$function_name`.")
end

function load_from_yaml(args...; kwargs...)
    return extension_not_loaded_error("YAML", "load_from_yaml")
end

function write_to_yaml(args...; kwargs...)
    return extension_not_loaded_error("YAML", "write_to_yaml")
end

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
* `base_module`: The module from which type and enum value names should be written.
  Default: Main
* `indent`: Determines the number of spaces used when pretty-printing JSON. Default: 4.
"""
function write_to_json end

function load_from_json(args...; kwargs...)
    return extension_not_loaded_error("JSON", "load_from_json")
end

function write_to_json(args...; kwargs...)
    return extension_not_loaded_error("JSON", "write_to_json")
end

# These generics are implemented by format extensions. They keep parser-specific behavior
# at the edge: the core package only knows how to turn dictionaries into structs and structs
# into dictionaries.
function load_yaml_dict end
function write_yaml_dict end
function load_json_dict end
function write_json_dict end

function load_yaml_dict(args...; kwargs...)
    return extension_not_loaded_error("YAML", "load_yaml_dict")
end

function write_yaml_dict(args...; kwargs...)
    return extension_not_loaded_error("YAML", "write_yaml_dict")
end

function load_json_dict(args...; kwargs...)
    return extension_not_loaded_error("JSON", "load_json_dict")
end

function write_json_dict(args...; kwargs...)
    return extension_not_loaded_error("JSON", "write_json_dict")
end

###########
# to_dict #
###########

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
to_dict(v::Function; kwargs...) = repr(v)
to_dict(v::AbstractVector; kwargs...) = [to_dict(el; kwargs...) for el in v]
to_dict(v::Tuple; kwargs...) = [to_dict(el; kwargs...) for el in v]
to_dict(v::NamedTuple; kwargs...) = OrderedDict(string(k) => to_dict(el; kwargs...) for (k, el) in pairs(v))
to_dict(v::AbstractDict; kwargs...) = OrderedDict(string(k) => to_dict(el; kwargs...) for (k, el) in pairs(v))

function module_path_from(base_module::Module, module_name::Module)
    module_path = fullname(module_name)
    base_path = fullname(base_module)
    if (
        length(module_path) >= length(base_path) &&
        module_path[1:length(base_path)] == base_path
    )
        return module_path[length(base_path)+1:end]
    else
        return module_path
    end
end

function enum_tag(v::Enum; base_module)
    module_path = string.(module_path_from(base_module, parentmodule(typeof(v))))
    return join((module_path..., string(v)), ".")
end

to_dict(v::Enum; base_module = Main, kwargs...) = enum_tag(v; base_module)

function binding_tag(value, base_module::Module, binding_symbol::Symbol)

    # If the requested base module already has this exact binding, use that local name.
    # This preserves compact tags like `Xoshiro` when a script imports `Random: Xoshiro`
    # and writes with the default `base_module = Main`.
    if (
        isdefined(base_module, binding_symbol) &&
        getfield(base_module, binding_symbol) === value
    )
        return string(binding_symbol)
    end

    # Otherwise, write the path from the requested base module to the binding's defining
    # module. This lets package code choose `base_module = @__MODULE__` and write its own
    # types as package-relative tags that can be loaded from the same base module later.
    module_path = string.(module_path_from(base_module, parentmodule(value)))
    return join((module_path..., string(binding_symbol)), ".")

end

# Try to figure out the type. This will search for Module.Submodule.Type. Any type
# parameters will be dropped. Dropping parameters is intentional here: when loading, field
# annotations and constructors usually reconstruct concrete parameters from the child
# values. Keeping this in one helper makes that policy easy to revisit.
function type_tag(v; base_module = Main)
    type = typeof(v)
    return binding_tag(type, base_module, nameof(type))
end

# The generic write path mirrors the generic load path: emit a type tag, then emit one
# recursively encoded entry per field. Specialized `to_dict` methods can replace this for
# compact or semantic representations, such as storing a filename instead of a large
# payload.
function to_dict(v; type_key, base_module = Main, kwargs...)
    dict = OrderedDict{String, Any}(type_key => type_tag(v; base_module))
    for fn in fieldnames(typeof(v))
        dict[string(fn)] = to_dict(getfield(v, fn); type_key, base_module, kwargs...)
    end
    return dict
end

###############
# Other Types #
###############

include("matrix.jl")

end # module PortableStructs
