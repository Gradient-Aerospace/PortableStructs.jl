using Test
using StaticArrays
using PortableStructs

@enum Fruit guava cantaloupe

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
end

@kwdef struct MyTypeWithAFieldCalledType
    type::String
    x::Float64
end

@kwdef struct MyManualType
    a::Rational{Int64}
    b::Complex{Float64}
end

# Here, we know what the "left hand side" is supposed to be for all fields, so this is the
# easy stuff.
@testset "concrete type of types" begin

    # Create an instance of the type and write it to YAML.
    x = MyConcreteType(
        1., 2, "3", [4., 5.], 6//1, 7. + 8im, nothing, missing, 'M', guava, SA[9., 10., 11.]
    )
    mkpath("out")
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
        SA[9., 10., 11.]
    )
    y = TypeWithMoreComplexFields(
        x,
        [x, x],
        (x, x),
        ("hi", x),
        [1., 2, "3", [4., 5.], 6//1, 7. + 8im, x],
        nothing, x, 1.,
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

end
