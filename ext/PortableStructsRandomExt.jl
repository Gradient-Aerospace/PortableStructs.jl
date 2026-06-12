module PortableStructsRandomExt

import PortableStructs
using Random: Xoshiro

# Xoshiro is an external collection of RNG state, so its reconstruction policy lives in an
# extension rather than the format-agnostic struct codec itself. The method reconstructs
# Xoshiro from its stored fields, which is necessarily tied to the type's field layout.
function PortableStructs.from_dict(
    ::Type{Xoshiro},
    v::AbstractDict{<:AbstractString, <:Any};
    kwargs...,
)
    return Xoshiro(
        (
            PortableStructs.from_dict(ft, v[string(fn)]; kwargs...)
            for (ft, fn) in zip(fieldtypes(Xoshiro), fieldnames(Xoshiro))
        )...,
    )
end

end # module PortableStructsRandomExt
