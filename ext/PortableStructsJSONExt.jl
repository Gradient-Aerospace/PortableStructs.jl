module PortableStructsJSONExt

import JSON
import PortableStructs
using OrderedCollections: OrderedDict

# JSON-specific behavior lives at this edge. The rest of PortableStructs works with plain
# dictionaries, so this extension's job is only to read JSON into those dictionaries and
# write dictionaries back out as JSON.
function read_json_dict(filename::AbstractString)
    return JSON.parsefile(filename; dicttype = OrderedDict{String, Any})
end

function PortableStructs.load_json_dict(filename::AbstractString; include_key = "include")
    dict = read_json_dict(filename)
    return PortableStructs.expand_include_files(
        dict,
        dirname(filename),
        read_json_dict;
        include_key,
    )
end

function PortableStructs.load_from_json(
    filename::AbstractString,
    t::Type;
    type_key = "type",
    base_module = Main,
    include_key = "include",
)
    dict = PortableStructs.load_json_dict(filename; include_key)
    return PortableStructs.from_dict(t, dict; type_key, base_module)
end

function PortableStructs.load_from_json(filename::AbstractString; kwargs...)
    return PortableStructs.load_from_json(filename, Any; kwargs...)
end

function PortableStructs.write_json_dict(filename::AbstractString, dict; indent = 4)
    open(filename, "w") do io
        JSON.print(io, dict, indent)
        println(io)
    end
    return nothing
end

function PortableStructs.write_to_json(
    filename::AbstractString,
    v;
    type_key = "type",
    base_module = Main,
    indent = 4,
)
    dict = PortableStructs.to_dict(v; type_key, base_module)
    return PortableStructs.write_json_dict(filename, dict; indent)
end

end # module PortableStructsJSONExt
