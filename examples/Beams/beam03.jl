using Compat, CSoM

data = @compat Dict(
  # Beam(ndim, nst, nxe, nye, nip, direction, finite_element(nod, nodof), axisymmetric)
  :element_type => Beam(3, 1, 20, 1, :x, Line(2, 6), false),
  :properties => [2.0e6 1.0e6 1.0e6 3.0e5;],
  :x_coords => linspace(0, 4, 21),
  :support => [
    (1, [0 0 0 0 0 0])
    ],
  :loaded_nodes => [
    (21, [10000.0 1000.0 0.0 1000.0 0.0 0.0])],
  :penalty => 1e19
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

