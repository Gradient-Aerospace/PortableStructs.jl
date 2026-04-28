module PortableStructsStaticArraysExt

import PortableStructs
using StaticArrays: SVector

# SVector is an external collection type, so its reconstruction logic lives in an
# extension. Core only needs to understand ordinary Julia vectors/tuples/dicts and generic
# structs; this adapter teaches PortableStructs how to turn a parsed vector back into an
# SVector when StaticArrays is available.
function PortableStructs.from_dict(::Type{SVector{N, ET}}, v::Vector; kwargs...) where {N, ET}
    return SVector{N, ET}(PortableStructs.from_dict(ET, el; kwargs...) for el in v)
end

function PortableStructs.from_dict(t::Type{<:SVector}, v::Vector; kwargs...)
    els = [PortableStructs.from_dict(eltype(t), el; kwargs...) for el in v]
    return SVector{length(els), eltype(t)}(els)
end

end # module PortableStructsStaticArraysExt
