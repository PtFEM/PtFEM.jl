using CSoM

data = Dict(
  # Frame(nels, nn, ndim, nst, nip, finite_element(nod, nodof))
  :element_type => Frame(10, 6, 2, 1, 1, Line(2, 2)),
  :properties => [2.0e5;],
  :x_coords => [0.0, 4.0, 4.0, 8.0, 8.0, 12.0],
  :y_coords => [3.0, 0.0, 3.0, 3.0, 0.0, 0.0],
  :g_num => [1 1 3 3 3 2 2 5 4 5;2 3 4 5 2 4 5 4 6 6],
  :support => [
    (1, [0 0]),
    (2, [1 0])
    ],
  :loaded_nodes => [
    (6, [0.0 -10.0])],
  :penalty => 1e19
)

data |> display
println()

@time m = FE4_2(data)
println()

println("Displacements:")
m.displacements |> display
println()

println("Actions:")
m.actions |> display
println()

println("Axial forces:")
m.axial |> display
println()

