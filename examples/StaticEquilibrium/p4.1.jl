using Compat, CSoM

data = @compat Dict(
  :element_type => Beam(4, 1, :x, Line(2)),
  :properties => [1.0e5;],
  :x_coords => linspace(0, 1, 5),
  :support => [
    (5, [0])
    ],
  :loaded_nodes => [
    (1, [-0.625]),
    (2, [-1.25]),
    (3, [-1.25]),
    (4, [-1,.5]),
    (5, [-0.625]),
    ]
)

data |> display
println()

@time m = FEbeam(data)
println()

println("Displacements:")
m.displacements |> display
println()

println("Actions:")
m.actions |> display
println()
