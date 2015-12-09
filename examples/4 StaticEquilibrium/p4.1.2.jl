using Compat, CSoM
include("FE4_1.jl")

data = @compat Dict(
  # Rod(nels, np_types, nip, finite_element(nod, nodof))
  :element_type => Rod(4, 1, 1, Line(2, 1)),
  :properties => [2.0e3; 1.0e3],
  :etype => [2, 2, 1, 1],
  :x_coords => linspace(0, 1, 5),
  :support => [
    (1, [0])
    ],
  :fixed_freedoms => [
    (5, 1, 0.05)
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
