########################
# Decoding Diagnostics #
########################

# Keep paths as their original keys and one-based indices until they need to be displayed.
# This avoids ambiguities from repeatedly concatenating path strings during recursion.
Base.@kwdef struct FromDictContext
    source::Union{Nothing, String} = nothing
    path::Tuple = ()
    tagged_as::Any = nothing
    type_tag::Any = nothing
    resolved_tag::Any = nothing
end

function child_context(context::FromDictContext, segment)
    return FromDictContext(;
        source = context.source,
        path = (context.path..., segment),
    )
end

function tagged_context(context::FromDictContext, requested_type, type_tag, resolved_tag)
    return FromDictContext(;
        source = context.source,
        path = context.path,
        tagged_as = requested_type,
        type_tag,
        resolved_tag,
    )
end

# Store a compact description rather than the complete input value, which may be very large.
struct FromDictError <: Exception
    context::FromDictContext
    requested_type::Any
    input_type::Type
    input_keys::Any
    reason::Union{Nothing, String}
    cause::Any
    cause_backtrace::Any
end

function FromDictError(
    context::FromDictContext,
    requested_type,
    value;
    reason::Union{Nothing, AbstractString} = nothing,
    cause = nothing,
    cause_backtrace = nothing,
)
    input_keys = value isa AbstractDict ? collect(keys(value)) : nothing
    reason_string = isnothing(reason) ? nothing : String(reason)
    return FromDictError(
        context,
        requested_type,
        typeof(value),
        input_keys,
        reason_string,
        cause,
        cause_backtrace,
    )
end

function show_path(io::IO, path::Tuple)

    if isempty(path)
        print(io, "<root>")
        return nothing
    end

    is_first = true
    for segment in path
        if segment isa Integer
            print(io, "[$segment]")
        else
            !is_first && print(io, "/")
            segment_text = string(segment)
            if any(character -> character in ('/', '[', ']'), segment_text)
                print(io, repr(segment_text))
            else
                print(io, segment_text)
            end
        end
        is_first = false
    end

    return nothing

end

function Base.showerror(io::IO, exception::FromDictError)

    print(io, "Could not construct $(exception.requested_type).")
    if !isnothing(exception.context.source)
        print(io, "\nSource: $(exception.context.source)")
    end
    print(io, "\nPath: ")
    show_path(io, exception.context.path)
    print(io, "\nInput type: $(exception.input_type)")

    if !isnothing(exception.context.type_tag)
        print(io, "\nType tag: $(repr(exception.context.type_tag))")
        if !isnothing(exception.context.resolved_tag)
            print(io, " resolved to $(exception.context.resolved_tag)")
        end
        if exception.context.tagged_as != exception.requested_type
            print(io, "\nThe tagged value was requested as $(exception.context.tagged_as).")
        end
    end

    if !isnothing(exception.input_keys)
        print(io, "\nInput keys: $(exception.input_keys)")
    end
    if !isnothing(exception.reason)
        print(io, "\n\n$(exception.reason)")
    end
    if !isnothing(exception.cause)
        print(io, "\n\nCaused by: ")
        showerror(io, exception.cause)
    end

end

function contextualize(f::Function, requested_type, value, context::FromDictContext)

    # Leave the catch block before throwing the wrapper. Otherwise Julia also records an
    # automatic exception chain, duplicating the cause that `showerror` renders above.
    captured_exception = try
        return f()
    catch exception
        (exception, catch_backtrace())
    end
    exception, backtrace = captured_exception
    exception isa FromDictError && throw(exception)
    throw(FromDictError(
        context,
        requested_type,
        value;
        cause = exception,
        cause_backtrace = backtrace,
    ))

end

# These are the only helpers recursive decoders need: decode at the current location, or
# append one or more dictionary keys/one-based indices before decoding a child.
function decode_value(t, value; _context = FromDictContext(), kwargs...)
    return contextualize(t, value, _context) do
        from_dict(t, value; _context, kwargs...)
    end
end

function decode_child(t, value, segments...; _context = FromDictContext(), kwargs...)
    context = foldl(child_context, segments; init = _context)
    return decode_value(t, value; _context = context, kwargs...)
end

function dictionary_construction_failure(type, dict; type_key, context)

    supplied_keys = [key for key in keys(dict) if key != type_key]
    lines = ["No keyword constructor accepts the supplied keys."]

    field_names = try
        fieldnames(type)
    catch
        nothing
    end
    if isnothing(field_names)
        push!(lines, "Positional construction could not identify a field layout to use.")
    else
        supplied_names = Symbol.(supplied_keys)
        missing_names = setdiff(field_names, supplied_names)
        unexpected_names = setdiff(supplied_names, field_names)
        push!(
            lines,
            "Positional construction requires the input keys to match the fields exactly.",
            "Expected fields: $field_names",
        )
        !isempty(missing_names) && push!(lines, "Missing fields: $missing_names")
        !isempty(unexpected_names) && push!(lines, "Unexpected fields: $unexpected_names")
    end

    ambiguous_type = type === Any || try
        isabstracttype(type)
    catch
        false
    end
    if isnothing(context.type_tag) && ambiguous_type
        push!(lines, "A \"$type_key\" key naming a concrete type may be required here.")
    end

    return join(lines, '\n')

end
