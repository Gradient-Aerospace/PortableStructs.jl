using Test
using StaticArrays
using PortableStructs
using Random: Xoshiro
import Dates
using OrderedCollections: OrderedDict
using YAML: load_file
using JSON

@enum Fruit guava cantaloupe

module EnumXFixtures
    using EnumX
    @enumx ScopedEnum scoped_value other_value
    @enumx OtherScopedEnum scoped_value
end

module NestedEnumFixtures
    module Nested
        @enum NestedFruit nested_apple nested_pear
    end
end

struct MyParseableType
    int::Int64
end
Base.parse(::Type{MyParseableType}, s::AbstractString) = MyParseableType(parse(Int64, s))

@kwdef struct MyConcreteType
    a::Float64
    b::Int64
    c::String
    d::Vector{Float64}
    e::Rational{Int64}
    f::Complex{Float64}
    g::Nothing
    h::Missing
    i::Char
    j::Fruit
    k::SVector{3, Float64}
    l::NamedTuple{(:x, :y), Tuple{Int64, Float64}}
    m::Symbol
    o::Xoshiro
    p::UInt64
end

@kwdef struct TypeWithEnumX
    value::EnumXFixtures.ScopedEnum.T
end

@kwdef struct TypeWithNestedEnum
    value::NestedEnumFixtures.Nested.NestedFruit
end

# We use this as a type that PackageArtifactFixtures doesn't know about in the base_module
# testing.
module OtherModule
    @kwdef struct OtherType
        value::Int64
    end
end

module EmptyBaseModule
end

# This tests reading and writing "from" a module, where the base_module is this module.
module PackageArtifactFixtures

    using PortableStructs

    @kwdef struct Leaf
        x::Int
    end

    # This type is parameterized so that we can make sure using types that this module
    # doesn't even know about is ok.
    @kwdef struct Wrapper{T}
        leaf::Leaf
        other::T
    end

    function write_yaml_artifact(file; other)
        artifact = Wrapper(; leaf = Leaf(; x = 7), other)
        PortableStructs.write_to_yaml(file, artifact; base_module = @__MODULE__)
        return nothing
    end

    function load_yaml_artifact(file)
        return PortableStructs.load_from_yaml(file; base_module = @__MODULE__)
    end

    function write_json_artifact(file; other)
        artifact = Wrapper(; leaf = Leaf(; x = 7), other)
        PortableStructs.write_to_json(file, artifact; base_module = @__MODULE__)
        return nothing
    end

    function load_json_artifact(file)
        return PortableStructs.load_from_json(file; base_module = @__MODULE__)
    end

end

function Base.:(==)(a::MyConcreteType, b::MyConcreteType)
    return all(
        ismissing(getfield(a, fn)) ? ismissing(getfield(b, fn)) :
        getfield(a, fn) == getfield(b, fn)
        for fn in fieldnames(MyConcreteType)
    )
end

@kwdef struct TypeWithMoreComplexFields{N}
    g::MyConcreteType
    h::Vector{MyConcreteType}
    i::NTuple{2, MyConcreteType}
    j::Tuple{String, MyConcreteType}
    k::Vector{Any}
    l::Union{Nothing, MyConcreteType}
    m::Union{Nothing, MyConcreteType}
    n::Real
    o::NamedTuple
    p::SVector{N, Symbol} # The type parameter means the size of this isn't known from the type when loading from YAML.
    q::NTuple{N, Float64}
end

@kwdef struct TypeWithMatrixFields
    floats::Matrix{Float64}
    ints::Matrix{Int}
    inferred::Matrix
end

@kwdef struct MyTypeWithAFieldCalledType
    type::String
    x::Float64
end

@kwdef struct MyManualType
    a::Rational{Int64}
    b::Complex{Float64}
    c::MyParseableType
    d::Tuple{Int64, String}
end

abstract type TaggedConfig end
abstract type OtherTaggedConfig end

# This intentionally does not have a keyword constructor. It exercises the fallback that
# constructs from positional arguments when dict keys exactly match field names.
struct PositionalOnly
    a::Int
    b::TaggedConfig
end

struct FieldMatchingButNoFieldConstructor
    x::Int
    FieldMatchingButNoFieldConstructor(x::String) = new(parse(Int, x))
end

function a_function_to_call(; x::Int64, y::String)
    return (x, y)
end

@kwdef struct Person
    name::String
    child::Union{Person, Nothing} = nothing
    sibling::Union{Person, Nothing} = nothing
end

# These tiny types exercise tagged dictionary loading without leaning on the larger
# round-trip fixtures below.
@kwdef struct TaggedConfigLeaf <: TaggedConfig
    x::Int
end

# This intentionally has no constructor. It pins down that PortableStructs only chooses a
# concrete implementation for `AbstractDict` itself, not for arbitrary abstract subtypes.
abstract type MyAbstractDict{K, V} <: AbstractDict{K, V} end

# Suppose we wanted a type that didn't write all of its fields to YAML. Consider a type
# that contains a big data vector loaded from a file. We don't need to save all of that data
# and could instead save just the file name.
struct CustomRepresentation
    filename::String
    contents::Vector{UInt8}
end
function CustomRepresentation(; filename)
    # "Load the data." (No need to actually load a file here.)
    contents = zeros(UInt8, length(filename))
    return CustomRepresentation(filename, contents)
end
function PortableStructs.to_dict(c::CustomRepresentation; type_key, kwargs...)
    return OrderedDict(
        type_key => "CustomRepresentation",
        "filename" => c.filename, # Save just the file name.
    )
end
function PortableStructs.from_dict(::Type{CustomRepresentation}, d::AbstractDict; kwargs...)
    # When loading, re-load the file.
    return CustomRepresentation(; filename = d["filename"])
end

# We'll put all of our output files here.
mkpath("out")

# Here, we know what the "left hand side" is supposed to be for all fields, so this is the
# easy stuff.
@testset "concrete type of types" begin

    # Create an instance of the type and write it to YAML.
    x = MyConcreteType(
        1., 2, "3", [4., 5.], 6//1, 7. + 8im, nothing, missing, 'M', guava,
        SA[9., 10., 11.], (; x = 1, y = 2.), :pika, Xoshiro(123), -0x1,
    )
    write_to_yaml("out/my_concrete_type.yaml", x)

    # Load in the YAML and see if everything's as it was.
    y = load_from_yaml("out/my_concrete_type.yaml")
    @test x == y

    # Do it again with a custom type_key.
    write_to_yaml("out/my_concrete_type_with_custom_type_key.yaml", x; type_key = "_type")

    # Load in the YAML and see if everything's as it was.
    y = load_from_yaml("out/my_concrete_type_with_custom_type_key.yaml"; type_key = "_type")
    @test x == y

end

@testset "base_module type tags" begin

    # We'll make a type that our PackageArtifactFixtures module doesn't know about.
    other = OtherModule.OtherType(1)

    # By default, type tags are written relative to Main. This keeps the ordinary script and
    # REPL path working, where user modules are commonly available below Main.
    default_file = "out/default_base_module_type_tag.yaml"
    default_artifact = PackageArtifactFixtures.Wrapper(;
        leaf = PackageArtifactFixtures.Leaf(; x = 3),
        other,
    )
    write_to_yaml(default_file, default_artifact)
    default_dict = load_file(default_file)
    @test default_dict["type"] == "PackageArtifactFixtures.Wrapper"
    @test default_dict["leaf"]["type"] == "PackageArtifactFixtures.Leaf"
    default_roundtrip = load_from_yaml(default_file)
    @test default_roundtrip.leaf.x == 3

    # Package code can choose its own module as the base. Then package-owned artifacts can
    # be written and reloaded without requiring the package module to be bound in Main.
    package_yaml_file = "out/package_base_module_type_tag.yaml"
    PackageArtifactFixtures.write_yaml_artifact(package_yaml_file; other)
    package_yaml_dict = load_file(package_yaml_file)
    @test package_yaml_dict["type"] == "Wrapper"
    @test package_yaml_dict["leaf"]["type"] == "Leaf"
    package_yaml_roundtrip = PackageArtifactFixtures.load_yaml_artifact(package_yaml_file)
    @test package_yaml_roundtrip.leaf.x == 7

    package_json_file = "out/package_base_module_type_tag.json"
    PackageArtifactFixtures.write_json_artifact(package_json_file; other)
    package_json_dict = JSON.parsefile(package_json_file)
    @test package_json_dict["type"] == "Wrapper"
    @test package_json_dict["leaf"]["type"] == "Leaf"
    package_json_roundtrip = PackageArtifactFixtures.load_json_artifact(package_json_file)
    @test package_json_roundtrip.leaf.x == 7

    # Types from loaded modules outside the base module are written with their root module
    # path. Loading should resolve the same path without requiring the root to be imported
    # into the chosen base module.
    date_file = "out/date_with_custom_base_module.yaml"
    date = Dates.Date(2026, 6, 24)
    write_to_yaml(date_file, date; base_module = EmptyBaseModule)
    date_dict = load_file(date_file)
    @test date_dict["type"] == "Dates.Date"
    @test !isdefined(EmptyBaseModule, :Dates)
    @test load_from_yaml(date_file; base_module = EmptyBaseModule) == date

end

@testset "JSON" begin

    x = MyConcreteType(
        1., 2, "3", [4., 5.], 6//1, 7. + 8im, nothing, missing, 'M', guava,
        SA[9., 10., 11.], (; x = 1, y = 2.), :pika, Xoshiro(123), -0x1,
    )
    file = "out/my_concrete_type.json"
    write_to_json(file, x)

    json = JSON.parsefile(file)
    @test json["type"] == "MyConcreteType"

    y = load_from_json(file)
    @test x == y

    manual_file = "out/manual.json"
    manual = OrderedDict(
        "a" => "1//2",
        "b" => "3.0 + 4.0im",
        "c" => "1",
        "d" => OrderedDict(
            "type" => "a_function_to_call",
            "x" => 1,
            "y" => "cats",
        ),
    )
    PortableStructs.write_json_dict(manual_file, manual)

    z = load_from_json(manual_file, MyManualType)
    @test z.a == 1//2
    @test z.b == 3.0 + 4im
    @test z.c isa MyParseableType
    @test z.c.int == 1
    @test z.d[1] == 1
    @test z.d[2] == "cats"

end

# This wouldn't work if we always relied on the type key to be "type", so this should test
# that in fact we're able to use a custom type key.
@testset "type_key" begin

    # Do it again with a custom type_key.
    x = MyTypeWithAFieldCalledType("Hello", pi)
    mkpath("out")
    write_to_yaml("out/my_type_with_a_field_called_type.yaml", x; type_key = "_type")

    # Load in the YAML and see if everything's as it was.
    y = load_from_yaml("out/my_type_with_a_field_called_type.yaml"; type_key = "_type")
    for fn in fieldnames(MyTypeWithAFieldCalledType)
        @test getfield(x, fn) == getfield(y, fn)
    end

end

@testset "scoped enums" begin

    type_key = "type"
    base_module = @__MODULE__

    # EnumX values are often referred to through their generated module. When the target
    # field type is already known, the loader should accept that scoped spelling.
    scoped_value = PortableStructs.from_dict(
        EnumXFixtures.ScopedEnum.T,
        "EnumXFixtures.ScopedEnum.scoped_value";
        type_key,
        base_module,
    )
    @test scoped_value === EnumXFixtures.ScopedEnum.scoped_value

    # The plain spelling should continue to work for generated EnumX values, just like it
    # does for Base.@enum values.
    plain_value = PortableStructs.from_dict(
        EnumXFixtures.ScopedEnum.T,
        "other_value";
        type_key,
        base_module,
    )
    @test plain_value === EnumXFixtures.ScopedEnum.other_value

    # This is the structured path that originally failed: a YAML scalar names a scoped
    # EnumX value, and the containing field annotation supplies the enum type.
    enumx_yaml = """
    value: EnumXFixtures.ScopedEnum.scoped_value
    """
    write("out/enumx.yaml", enumx_yaml)
    loaded = load_from_yaml("out/enumx.yaml", TypeWithEnumX; base_module)
    @test loaded.value === EnumXFixtures.ScopedEnum.scoped_value

    # Matching only the leaf binding name would incorrectly accept this value, since both
    # enum modules define `scoped_value`. A scoped string must resolve to the expected enum
    # type, not merely to an enum value with the same final name.
    wrong_enum_message = "\"EnumXFixtures.OtherScopedEnum.scoped_value\" did not map"
    @test_throws wrong_enum_message PortableStructs.from_dict(
        EnumXFixtures.ScopedEnum.T,
        "EnumXFixtures.OtherScopedEnum.scoped_value";
        type_key,
        base_module,
    )

    # Round-tripped EnumX values should be written as bindings relative to the same base
    # module used for loading.
    x = TypeWithEnumX(; value = EnumXFixtures.ScopedEnum.other_value)
    write_to_yaml("out/enumx_roundtrip.yaml", x; base_module)
    enumx_roundtrip_dict = load_file("out/enumx_roundtrip.yaml")
    @test enumx_roundtrip_dict["value"] == "EnumXFixtures.ScopedEnum.other_value"
    roundtrip = load_from_yaml("out/enumx_roundtrip.yaml"; base_module)
    @test roundtrip.value === x.value

    write_to_json("out/enumx_roundtrip.json", x; base_module)
    enumx_json_dict = JSON.parsefile("out/enumx_roundtrip.json")
    @test enumx_json_dict["value"] == "EnumXFixtures.ScopedEnum.other_value"
    json_roundtrip = load_from_json("out/enumx_roundtrip.json"; base_module)
    @test json_roundtrip.value === x.value

    # The same binding-path representation should work for ordinary `Base.@enum` values
    # inside nested modules.
    nested = TypeWithNestedEnum(;
        value = NestedEnumFixtures.Nested.nested_apple,
    )
    write_to_yaml("out/nested_base_enum_roundtrip.yaml", nested; base_module)
    nested_roundtrip_dict = load_file("out/nested_base_enum_roundtrip.yaml")
    @test nested_roundtrip_dict["value"] == "NestedEnumFixtures.Nested.nested_apple"
    nested_roundtrip = load_from_yaml(
        "out/nested_base_enum_roundtrip.yaml";
        base_module,
    )
    @test nested_roundtrip.value === nested.value

end

module ModuleA
    module ModuleA1
        @enum Fruit apple=71 banana
    end
end

@kwdef struct TypeWithModuleEnum
    value::ModuleA.ModuleA1.Fruit
end

@testset "enums in modules" begin
    enums_in_modules_yaml = """
    value: ModuleA.ModuleA1.apple
    """
    write("out/enums_in_modules.yaml", enums_in_modules_yaml)
    loaded = load_from_yaml(
        "out/enums_in_modules.yaml",
        TypeWithModuleEnum;
        base_module = @__MODULE__,
    )
    @test loaded.value === ModuleA.ModuleA1.apple
end

# Here, we test abstract types and unions, where we don't know what the left-hand side needs
# to be exactly.
@testset "more complex fields" begin

    x = MyConcreteType(
        1., 2, "3", [4., 5.], 6//1, 7. + 8im, nothing, missing, 'M', cantaloupe,
        SA[9., 10., 11.], (; x = 1, y = 2.), :pika, Xoshiro(123), -0x1,
    )
    y = TypeWithMoreComplexFields(
        x,
        [x, x],
        (x, x),
        ("hi", x),
        [1., 2, "3", [4., 5.], 6//1, 7. + 8im, x],
        nothing, x, 1.,
        (; z = "butternut squash", ),
        SA[:a, :b],
        (0., 1.),
    )

    mkpath("out")
    write_to_yaml("out/my_type_with_more_complex_fields.yaml", y)

    z = load_from_yaml("out/my_type_with_more_complex_fields.yaml")
    for fn in fieldnames(TypeWithMoreComplexFields)
        @test getfield(y, fn) == getfield(z, fn)
    end

end

@testset "matrix loading" begin

    type_key = "type"
    base_module = @__MODULE__
    matrix_dict(rows; type_key = "type") = OrderedDict(type_key => "Matrix", "rows" => rows)

    # Matrix dictionaries should recursively decode their rows into the requested element
    # type, including scalar conversions.
    floats = PortableStructs.from_dict(
        Matrix{Float64},
        matrix_dict(Any[Any[1.0, 2.5], Any[3.0, 4.0]]);
        type_key,
        base_module,
    )
    @test floats isa Matrix{Float64}
    @test floats == [1.0 2.5; 3.0 4.0]

    ints = PortableStructs.from_dict(
        Matrix{Int},
        matrix_dict(Any[Any["1", "2"], Any["3", "4"]]);
        type_key,
        base_module,
    )
    @test ints isa Matrix{Int}
    @test ints == [1 2; 3 4]

    # Bare Matrix annotations should infer the element type from decoded row entries.
    inferred = PortableStructs.from_dict(
        Matrix,
        matrix_dict([[1.0, 2.0], [3.0, 4.0]]);
        type_key,
        base_module,
    )
    @test inferred isa Matrix{Float64}
    @test inferred == [1.0 2.0; 3.0 4.0]

    # Matrix entries should preserve the existing tagged abstract-type loading behavior.
    tagged_leaf = OrderedDict(type_key => "TaggedConfigLeaf", "x" => 7)
    tagged = PortableStructs.from_dict(
        Matrix{TaggedConfig},
        matrix_dict([[tagged_leaf, tagged_leaf]]);
        type_key,
        base_module,
    )
    @test tagged isa Matrix{TaggedConfig}
    @test tagged[1, 1] isa TaggedConfigLeaf
    @test tagged[1, 1].x == 7

    # Matrix dictionaries should fail clearly when the required row structure is invalid.
    @test_throws "each row must be a vector" PortableStructs.from_dict(
        Matrix{Float64},
        matrix_dict(Any[Any[1.0, 2.0], 3.0]);
        type_key,
        base_module,
    )
    @test_throws "all rows must have the same length" PortableStructs.from_dict(
        Matrix{Float64},
        matrix_dict(Any[Any[1.0, 2.0], Any[3.0]]);
        type_key,
        base_module,
    )
    @test_throws "rows must be a vector" PortableStructs.from_dict(
        Matrix{Float64},
        OrderedDict(type_key => "Matrix", "rows" => 1.0);
        type_key,
        base_module,
    )
    @test_throws "\"rows\" key was missing" PortableStructs.from_dict(
        Matrix{Float64},
        OrderedDict(type_key => "Matrix");
        type_key,
        base_module,
    )
    vector_tagged_matrix = OrderedDict(
        type_key => "Vector",
        "rows"   => Any[Any[1.0, 2.0]],
    )
    matrix_type_error = "Could not construct a matrix from a \"Vector\" type tag"
    @test_throws matrix_type_error PortableStructs.from_dict(
        Matrix{Float64},
        vector_tagged_matrix;
        type_key,
        base_module,
    )

    # YAML loading should use the canonical explicit representation for matrix fields.
    mkpath("out")
    matrix_yaml = """
    floats:
      type: Matrix
      rows:
        - [1.0, 2.5]
        - [3.0, 4.0]
    ints:
      type: Matrix
      rows:
        - ["1", "2"]
        - ["3", "4"]
    inferred:
      type: Matrix
      rows:
        - [1.0, 2.0]
        - [3.0, 4.0]
    """
    write("out/matrix_fields.yaml", matrix_yaml)

    loaded = load_from_yaml(
        "out/matrix_fields.yaml",
        TypeWithMatrixFields;
        base_module,
    )
    @test loaded.floats isa Matrix{Float64}
    @test loaded.floats == [1.0 2.5; 3.0 4.0]
    @test loaded.ints isa Matrix{Int}
    @test loaded.ints == [1 2; 3 4]
    @test loaded.inferred isa Matrix{Float64}
    @test loaded.inferred == [1.0 2.0; 3.0 4.0]

    # Roundtrip writing should preserve matrix intent instead of writing vector-of-vector
    # row lists that would load ambiguously without a field annotation.
    write_to_yaml("out/matrix_fields_roundtrip.yaml", loaded)
    roundtrip_dict = load_file("out/matrix_fields_roundtrip.yaml")
    @test roundtrip_dict["floats"][type_key] == "Matrix"
    @test roundtrip_dict["floats"]["rows"] == [[1.0, 2.5], [3.0, 4.0]]
    @test roundtrip_dict["ints"][type_key] == "Matrix"
    @test roundtrip_dict["ints"]["rows"] == [[1, 2], [3, 4]]
    @test roundtrip_dict["inferred"][type_key] == "Matrix"
    @test roundtrip_dict["inferred"]["rows"] == [[1.0, 2.0], [3.0, 4.0]]

    roundtrip = load_from_yaml(
        "out/matrix_fields_roundtrip.yaml",
        TypeWithMatrixFields;
        base_module,
    )
    @test roundtrip.floats == loaded.floats
    @test roundtrip.ints == loaded.ints
    @test roundtrip.inferred == loaded.inferred

    # Matrix serialization should honor a custom type key just like other tagged values.
    write_to_yaml("out/matrix_fields_custom_type_key.yaml", loaded; type_key = "_type")
    custom_type_key_dict = load_file("out/matrix_fields_custom_type_key.yaml")
    @test custom_type_key_dict["floats"]["_type"] == "Matrix"
    @test custom_type_key_dict["floats"]["rows"] == [[1.0, 2.5], [3.0, 4.0]]

end

@testset "generic tuple and dictionary loading" begin

    type_key = "type"
    base_module = @__MODULE__
    tagged_leaf = OrderedDict(
        type_key => "TaggedConfigLeaf",
        "x" => 2,
    )

    # A bare `Tuple` annotation has no element types, so children should be decoded as `Any`.
    # This still needs to recurse into tagged children rather than returning raw dicts.
    tuple = PortableStructs.from_dict(Tuple, Any[1, tagged_leaf]; type_key, base_module)
    @test tuple[1] == 1
    @test tuple[2] isa TaggedConfigLeaf
    @test tuple[2].x == 2

    # A tagged dictionary can identify a tuple inside a field whose declared type provides
    # no more information than `Any`. Its args use the same recursively decoded vector path
    # tested above.
    tagged_tuple = PortableStructs.from_dict(
        Any,
        OrderedDict(
            type_key => "Tuple",
            "args" => Any[1, tagged_leaf],
        );
        type_key,
        base_module,
    )
    @test tagged_tuple[1] == 1
    @test tagged_tuple[2] isa TaggedConfigLeaf
    @test tagged_tuple[2].x == 2

    # The same representation works when the caller already knows that the target is a
    # Tuple, in which case no type tag is necessary.
    known_tuple = PortableStructs.from_dict(
        Tuple,
        OrderedDict("args" => Any[3, "four"]);
        type_key,
        base_module,
    )
    @test known_tuple == (3, "four")

    # Exercise the format-specific YAML loader rather than only testing its decoded
    # dictionary representation.
    @test load_from_yaml("tuple.yaml"; base_module) == (1024, 768)

    # Malformed tuple dictionaries produce errors specific to this representation.
    @test_throws "\"args\" key was missing" PortableStructs.from_dict(
        Tuple,
        OrderedDict{String, Any}();
        type_key,
        base_module,
    )
    @test_throws "must be a sequence" PortableStructs.from_dict(
        Tuple,
        OrderedDict("args" => 1);
        type_key,
        base_module,
    )
    @test_throws "may only contain" PortableStructs.from_dict(
        Tuple,
        OrderedDict("args" => Any[1, 2], "extra" => true);
        type_key,
        base_module,
    )
    @test_throws "Could not construct a Tuple" PortableStructs.from_dict(
        Tuple,
        OrderedDict(type_key => "NamedTuple", "args" => Any[1, 2]);
        type_key,
        base_module,
    )

    # A fully typed tuple should reject the wrong number of elements instead of silently
    # truncating through `zip(fieldtypes(T), v)`.
    @test_throws AssertionError PortableStructs.from_dict(
        Tuple{Int, String},
        Any[1];
        type_key,
        base_module,
    )

    # Generic NamedTuple loading should use parsed keys as field names, skip the type tag,
    # and recurse into tagged child values.
    named_tuple = PortableStructs.from_dict(
        NamedTuple,
        OrderedDict(
            type_key => "ignored",
            "leaf" => tagged_leaf,
            "count" => 3,
        );
        type_key,
        base_module,
    )
    @test keys(named_tuple) == (:leaf, :count)
    @test named_tuple.leaf isa TaggedConfigLeaf
    @test named_tuple.leaf.x == 2
    @test named_tuple.count == 3

    # Types without keyword constructors can still load when the dict keys exactly match
    # field names. The input order should not matter; construction uses the type's field
    # order.
    positional = PortableStructs.from_dict(
        PositionalOnly,
        OrderedDict(
            "b" => tagged_leaf,
            "a" => "5",
        );
        type_key,
        base_module,
    )
    @test positional.a == 5
    @test positional.b isa TaggedConfigLeaf
    @test positional.b.x == 2

    # Unparameterized parametric types can still expose a definite field layout. UnitRange
    # is not concrete, but its "start" and "stop" fields map directly to its positional
    # constructor.
    unit_range = PortableStructs.from_dict(
        UnitRange,
        OrderedDict(
            "stop" => 2,
            "start" => 1,
        );
        type_key,
        base_module,
    )
    @test unit_range == 1:2
    @test unit_range isa UnitRange{Int}

    # Positional fallback should stay narrow: extra or missing keys mean the dict does not
    # unambiguously map to the type's fields.
    @test_throws "Could not construct" PortableStructs.from_dict(
        PositionalOnly,
        OrderedDict(
            "a" => "5",
            "b" => tagged_leaf,
            "extra" => true,
        );
        type_key,
        base_module,
    )

    # Matching the field names alone is not enough; there still needs to be a positional
    # constructor that accepts the decoded field values.
    no_constructor_message = "No positional constructor accepts"
    @test_throws no_constructor_message PortableStructs.from_dict(
        FieldMatchingButNoFieldConstructor,
        OrderedDict("x" => "5");
        type_key,
        base_module,
    )

    source_dict = OrderedDict(
        type_key => "ignored",
        "a" => "1",
        "b" => 2,
    )

    # The exact `AbstractDict` interface can materialize as the package's default concrete
    # mapping type while preserving recursive decoding and filtering the type tag.
    abstract_dict = PortableStructs.from_dict(
        AbstractDict,
        source_dict;
        type_key,
        base_module,
    )
    @test abstract_dict isa OrderedDict
    @test !haskey(abstract_dict, type_key)
    @test abstract_dict["a"] == "1"
    @test abstract_dict["b"] == 2

    # A parameterized AbstractDict should still materialize as OrderedDict, but it should
    # honor the requested key and value types.
    typed_abstract_dict = PortableStructs.from_dict(
        AbstractDict{String, Int},
        source_dict;
        type_key,
        base_module,
    )
    @test typed_abstract_dict isa OrderedDict{String, Int}
    @test typed_abstract_dict == OrderedDict{String, Int}("a" => 1, "b" => 2)

    # An unparameterized Dict should be constructed as a Dict, infer key/value types, and
    # continue recursive decoding for tagged child values.
    dict = PortableStructs.from_dict(
        Dict,
        OrderedDict(
            type_key => "ignored",
            "leaf" => tagged_leaf,
        );
        type_key,
        base_module,
    )
    @test dict isa Dict
    @test !haskey(dict, type_key)
    @test dict["leaf"] isa TaggedConfigLeaf
    @test dict["leaf"].x == 2

    # A fully parameterized Dict should honor its requested value type rather than simply
    # copying parser-produced values through.
    typed_dict = PortableStructs.from_dict(
        Dict{String, Float64},
        OrderedDict(
            type_key => "ignored",
            "a" => 1.0,
            "b" => "2.5",
        );
        type_key,
        base_module,
    )
    @test typed_dict isa Dict{String, Float64}
    @test typed_dict == Dict{String, Float64}("a" => 1.0, "b" => 2.5)

    # OrderedDict gets its own concrete construction path so ordered inputs keep their order.
    ordered_dict = PortableStructs.from_dict(
        OrderedDict{String, Int},
        source_dict;
        type_key,
        base_module,
    )
    @test ordered_dict isa OrderedDict{String, Int}
    @test collect(keys(ordered_dict)) == ["a", "b"]
    @test ordered_dict == OrderedDict{String, Int}("a" => 1, "b" => 2)

    # PortableStructs should not guess a concrete type for arbitrary abstract dictionary
    # subtypes. Users can add a specific `from_dict` method for those.
    @test_throws "Could not construct" PortableStructs.from_dict(
        MyAbstractDict{String, Int},
        OrderedDict("a" => "1");
        type_key,
        base_module,
    )

end

@testset "tagged and dict-backed scalar loading" begin

    type_key = "type"
    base_module = @__MODULE__
    tagged_leaf = OrderedDict(
        type_key => "TaggedConfigLeaf",
        "x" => 4,
    )

    # Tagged values should resolve to the concrete tag and still satisfy the requested
    # abstract type.
    config = PortableStructs.from_dict(TaggedConfig, tagged_leaf; type_key, base_module)
    @test config isa TaggedConfigLeaf
    @test config.x == 4

    # The final requested type check should reject a valid tag that resolves to the wrong
    # branch of the type hierarchy.
    @test_throws MethodError PortableStructs.from_dict(
        OtherTaggedConfig,
        tagged_leaf;
        type_key,
        base_module,
    )

    # Rational values can be represented as field dictionaries, both when the element type is
    # known and when it should be inferred by Julia's Rational constructor.
    @test PortableStructs.from_dict(
        Rational{Int},
        OrderedDict("num" => 1.0, "den" => 2.0);
        type_key,
        base_module,
    ) === 1//2

    @test PortableStructs.from_dict(
        Rational,
        OrderedDict("num" => 3, "den" => 4);
        type_key,
        base_module,
    ) === 3//4

    # Complex values have the same dict-backed path as Rational values, including support for
    # either explicit or inferred element types.
    complex_value = PortableStructs.from_dict(
        Complex{Float32},
        OrderedDict("re" => 1, "im" => 2);
        type_key,
        base_module,
    )
    @test complex_value === Complex{Float32}(1, 2)

    @test PortableStructs.from_dict(
        Complex,
        OrderedDict("re" => 3, "im" => 4);
        type_key,
        base_module,
    ) === 3 + 4im

end

# There are things we can load that aren't how *we* encode them. We want to support those
# things too.
@testset "manual YAML files" begin

    # This has string encodings for rationals and complex values, and it doesn't say which
    # type it is, so we need to use the type input.
    x = load_from_yaml("manual.yaml", MyManualType)
    @test x.a == 1//2
    @test x.b == 3.0 + 4im
    @test x.c isa MyParseableType
    @test x.c.int == 1
    @test x.d[1] == 1
    @test x.d[2] == "cats"

    # Check that "include" works as advertised through multiple directories and local paths.
    grandma = load_from_yaml("grandma.yaml", Person; include_key = "_include")
    @test grandma.name == "Grandma"
    @test grandma.child.name == "Parent"
    @test grandma.child.sibling.name == "Sis" # Tests that we can overwrite an include.
    @test grandma.child.child.name == "Grandchild 1"
    @test grandma.child.child.sibling.name == "Grandchild 2"

    vector_includes = load_from_yaml("vector_include.yaml")
    @test vector_includes["items"][1]["name"] == "expanded"
    @test !haskey(vector_includes["items"][1], "include")

end

@testset "exceptions" begin

    x = load_from_yaml("exceptions/exceptions.yaml")
    @test x["rosaceae"]["pyrus"] == ["communis"]
    @test x["rosaceae"]["malus"] == ["domestica", "fusca"]
    @test x["rosaceae"]["sorbus"] == ["aucuparia", "reducta", "pratti"]
    @test x["rosaceae"]["notes"] == "There are a lot more species to add here."

    y = load_from_yaml("exceptions/relative_exception.yaml")
    @test y["rosaceae"]["sorbus"] == ["aucuparia", "reducta", "pratti"]

    trees = load_from_yaml("exceptions/tree_exception.yaml")
    @test length(trees["trees"]) == 2
    @test trees["trees"][1]["common_name"] == "strawberry tree"
    @test trees["trees"][2]["scientific_name"] == "Arbutus menziesii"
    @test trees["trees"][2]["common_name"] == "madrona"

    @test_throws "While overwriting the value in \"rosac7eae.malus\"" load_from_yaml("exceptions/bad_exceptions.yaml")
    @test_throws "\"arctostaphylos\" is not a valid key." load_from_yaml("exceptions/more_bad_exceptions.yaml")
    @test_throws "index 3 was not found" load_from_yaml("exceptions/bad_vector_exception.yaml")
    @test_throws "a non-positive index was encountered" load_from_yaml("exceptions/bad_zero_vector_exception.yaml")

end

@testset "custom to_dict and from_dict" begin

    file = "out/my_custom_type.yaml"
    c = CustomRepresentation(; filename = "abalone.txt")
    write_to_yaml(file, c)
    yaml = load_file(file)
    @test length(keys(yaml)) == 2
    @test haskey(yaml, "type")
    @test haskey(yaml, "filename")
    c2 = load_from_yaml(file, CustomRepresentation)
    @test c.filename == c2.filename
    @test c.contents == c2.contents

end

module TestA
    module TestB
        function foo(x)
            return 2x
        end
    end
end

@kwdef struct FunctionWrapper
    f::Function
end

@testset "function" begin

    # Test a function embedded in a bunch of modules.
    file = "out/my_function.yaml"
    fw = FunctionWrapper(; f = TestA.TestB.foo)
    write_to_yaml(file, fw)
    fw2 = load_from_yaml(file)
    @test fw2.f == fw.f

    # Test that an anonymous function is recognized and errors out.
    file = "out/my_anonymous_function.yaml"
    fw = FunctionWrapper(; f = (x) -> 3x)
    write_to_yaml(file, fw)
    @test_throws "Could not load an anonymous function" load_from_yaml(file)

end
