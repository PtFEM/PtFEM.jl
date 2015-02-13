using Compat, CSoM
include("FE4_1.jl")

data = @compat Dict(
  # Beam(ndim, nst, nxe, nye, nip, direction, finite_element(nod, nodof), axisymmetric)
  :element_type => Beam(1, 1, 4, 1, :x, Line(2, 1), false),
  :properties => [2.0e3; 1.0e3],
  :etype => [2, 2, 1, 1],
  :x_coords => linspace(0, 1, 5),
  :support => [
    (1, [0])
    ],
  :fixed_freedoms => [
    (5, [0.05])
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
