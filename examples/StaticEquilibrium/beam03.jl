using Compat, CSoM

data = @compat Dict(
  :element_type => Beam(20, 1, :x, Line(2)),
  :properties => [2.0e6 1.0e6 1.0e6 3.0e5;],
  :x_coords => linspace(0, 4, 21),
  :support => [
    (1, [0 0 0 0 0 0])
    ],
  :loaded_nodes => [
    (21, [10000.0 1000.0 0.0 1000.0 0.0 0.0])]
)

data |> display
println()

println("Running Julia version:")
@time m = FEbeam(data)
println()

println("Displacements:")
m.displacements |> display
println()

println("Actions:")
m.actions |> display
println()

