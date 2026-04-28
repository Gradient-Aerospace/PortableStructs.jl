module PortableStructsRandomExt

import PortableStructs
using Random: Xoshiro

# Xoshiro has no keyword constructor, so the generic field-to-keyword path in core cannot
# rebuild it. This adapter belongs in an extension because it is support for one external
# type, not part of the format-agnostic struct codec itself.
#
# The method reconstructs Xoshiro from its stored fields. That is necessarily tied to the
# type's field layout, so keeping it isolated makes the maintenance risk obvious.
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
