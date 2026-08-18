module PortableStructsYAMLExt

import PortableStructs
import YAML
using OrderedCollections: OrderedDict

# YAML-specific behavior lives at this edge. The rest of PortableStructs works with plain
# dictionaries, so this extension's job is only to read YAML into those dictionaries and
# write dictionaries back out as YAML.
function read_yaml_dict(filename::AbstractString)
    return YAML.load_file(filename; dicttype = OrderedDict{String, Any})
end

function PortableStructs.load_yaml_dict(filename::AbstractString; include_key = "include")
    dict = read_yaml_dict(filename)
    return PortableStructs.expand_include_files(
        dict,
        dirname(filename),
        read_yaml_dict;
        include_key,
    )
end

function PortableStructs.load_from_yaml(
    filename::AbstractString,
    t::Type;
    type_key = "type",
    args_key = "args",
    kwargs_key = "kwargs",
    base_module = Main,
    include_key = "include",
)
    dict = PortableStructs.load_yaml_dict(filename; include_key)
    return PortableStructs.from_dict(
        t,
        dict;
        type_key,
        args_key,
        kwargs_key,
        base_module,
    )
end

function PortableStructs.load_from_yaml(filename::AbstractString; kwargs...)
    return PortableStructs.load_from_yaml(filename, Any; kwargs...)
end

function PortableStructs.write_yaml_dict(filename::AbstractString, dict)
    YAML.write_file(filename, dict)
    return nothing
end

function PortableStructs.write_to_yaml(
    filename::AbstractString,
    v;
    type_key = "type",
    base_module = Main,
)
    dict = PortableStructs.to_dict(v; type_key, base_module)
    return PortableStructs.write_yaml_dict(filename, dict)
end

end # module PortableStructsYAMLExt
