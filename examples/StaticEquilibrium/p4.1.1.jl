using Compat, CSoM
include("FE4_1.jl")

data = @compat Dict(
  # Beam(ndim, nst, nxe, nip, direction, finite_element(nod, nodof), axisymmetric)
  :element_type => Beam(1, 1, 4, 1, :x, Line(2, 1), false),
  :properties => [1.0e5;],
  :x_coords => linspace(0, 1, 5),
  :support => [
    (1, [0])
    ],
  :loaded_nodes => [
    (1, [-0.625]),
    (2, [-1.25]),
    (3, [-1.25]),
    (4, [-1.25]),
    (5, [-0.625])
    ]
)

data |> display
println()

@time m = FE4_1(data)
println()

println("Displacements:")
m.displacements |> display
println()

println("Actions:")
m.actions |> display
println()
