function matrix_rows(dict::AbstractDict)
    haskey(dict, "rows") ||
        error("Could not construct a matrix because the \"rows\" key was missing.")
    return dict["rows"]
end

function validate_matrix_type(dict::AbstractDict; type_key, base_module)

    # Generic tagged loading strips the type key before dispatching here. If the key is
    # still present, make sure this really is the matrix representation.
    if haskey(dict, type_key)
        target = resolve_constructor_tag(dict; type_key, base_module)
        target === Matrix ||
            error("Could not construct a matrix from a \"$(dict[type_key])\" type tag.")
    end

end

function matrix_column_count(rows)

    # The serialized form is row-major for readability, even though Julia matrices are
    # column-major in memory.
    rows isa AbstractVector ||
        error("Could not construct a matrix from row data; rows must be a vector.")
    isempty(rows) && return 0
    all(row -> row isa AbstractVector, rows) ||
        error("Could not construct a matrix from row data; each row must be a vector.")

    ncols = length(first(rows))
    all(row -> length(row) == ncols, rows) ||
        error(
            """
            Could not construct a matrix from row data; all rows must have the same length.
            """,
        )
    return ncols

end

function matrix_from_rows(::Type{T}, rows; kwargs...) where {T}

    # Element conversion still goes through `from_dict`, so typed matrices preserve the
    # same scalar, abstract-type, and tagged-value behavior as other containers.
    ncols = matrix_column_count(rows)
    matrix = Matrix{T}(undef, length(rows), ncols)
    for i in eachindex(rows), j in 1:ncols
        matrix[i, j] = from_dict(T, rows[i][j]; kwargs...)
    end
    return matrix

end

function inferred_matrix_from_rows(rows; kwargs...)
    ncols = matrix_column_count(rows)
    return [
        from_dict(Any, rows[i][j]; kwargs...)
        for i in eachindex(rows), j in 1:ncols
    ]
end

"""
    from_dict(::Type{<:Matrix}, dict::AbstractDict; kwargs...)

Constructs a matrix from the explicit PortableStructs matrix representation:

```
type: Matrix
rows:
  - [1, 2]
  - [3, 4]
```

The type key is configurable through the same `type_key` keyword used by the rest of
PortableStructs. The loader also accepts the inner dictionary after the generic
tagged-value path has consumed the type key.
"""
function from_dict(
    ::Type{Matrix{T}},
    dict::AbstractDict;
    type_key,
    base_module,
    kwargs...,
) where {T}

    validate_matrix_type(dict; type_key, base_module)
    return matrix_from_rows(
        T,
        matrix_rows(dict);
        type_key,
        base_module,
        kwargs...,
    )

end

function from_dict(::Type{Matrix}, dict::AbstractDict; type_key, base_module, kwargs...)
    validate_matrix_type(dict; type_key, base_module)
    return inferred_matrix_from_rows(
        matrix_rows(dict);
        type_key,
        base_module,
        kwargs...,
    )
end

function matrix_rows_to_dict(v::AbstractMatrix; kwargs...)
    return [
        [to_dict(v[i, j]; kwargs...) for j in axes(v, 2)]
        for i in axes(v, 1)
    ]
end

"""
    to_dict(v::Matrix; type_key, kwargs...)

Builds the explicit PortableStructs matrix representation, with serialized rows under a
`rows` key and a configurable matrix type tag.
"""
function to_dict(v::Matrix; type_key, kwargs...)
    return OrderedDict(
        type_key => "Matrix",
        "rows"   => matrix_rows_to_dict(v; type_key, kwargs...),
    )
end
