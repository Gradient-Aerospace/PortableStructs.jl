using Test
using StaticArrays
using PortableStructs
using Random: Xoshiro
using OrderedCollections: OrderedDict
using YAML: load_file

@enum Fruit guava cantaloupe

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
    o::Xoshiro
    p::UInt64
end

function Base.:(==)(a::MyConcreteType, b::MyConcreteType)
    return all(
        ismissing(getfield(a, fn)) ? ismissing(getfield(b, fn)) :
        getfield(a, fn) == getfield(b, fn)
        for fn in fieldnames(MyConcreteType)
    )
end

@kwdef struct TypeWithMoreComplexFields
    g::MyConcreteType
    h::Vector{MyConcreteType}
    i::NTuple{2, MyConcreteType}
    j::Tuple{String, MyConcreteType}
    k::Vector{Any}
    l::Union{Nothing, MyConcreteType}
    m::Union{Nothing, MyConcreteType}
    n::Real
    o::NamedTuple
end

@kwdef struct MyTypeWithAFieldCalledType
    type::String
    x::Float64
end

@kwdef struct MyManualType
    a::Rational{Int64}
    b::Complex{Float64}
    c::MyParseableType
end

@kwdef struct Person
    name::String
    child::Union{Person, Nothing} = nothing
    sibling::Union{Person, Nothing} = nothing
end

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
        SA[9., 10., 11.], (; x = 1, y = 2.), Xoshiro(123), -0x1,
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

# Here, we test abstract types and unions, where we don't know what the left-hand side needs
# to be exactly.
@testset "more complex fields" begin

    x = MyConcreteType(
        1., 2, "3", [4., 5.], 6//1, 7. + 8im, nothing, missing, 'M', cantaloupe,
        SA[9., 10., 11.], (; x = 1, y = 2.), Xoshiro(123), -0x1,
    )
    y = TypeWithMoreComplexFields(
        x,
        [x, x],
        (x, x),
        ("hi", x),
        [1., 2, "3", [4., 5.], 6//1, 7. + 8im, x],
        nothing, x, 1.,
        (; z = "butternut squash", ),
    )

    mkpath("out")
    write_to_yaml("out/my_type_with_more_complex_fields.yaml", y)

    z = load_from_yaml("out/my_type_with_more_complex_fields.yaml")
    for fn in fieldnames(TypeWithMoreComplexFields)
        @test getfield(y, fn) == getfield(z, fn)
    end

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

    # Check that "include" works as advertised through multiple directories and local paths.
    grandma = load_from_yaml("grandma.yaml", Person; include_key = "_include")
    @test grandma.name == "Grandma"
    @test grandma.child.name == "Parent"
    @test grandma.child.sibling.name == "Sis" # Tests that we can overwrite an include.
    @test grandma.child.child.name == "Grandchild 1"
    @test grandma.child.child.sibling.name == "Grandchild 2"

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
    @test c.contents == c2.contents

end
