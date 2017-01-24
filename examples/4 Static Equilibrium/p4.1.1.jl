using CSoM

data = Dict(
  # Rod(nels, np_types, nip, finite_element(nod, nodof))
  :element_type => Rod(4, 1, 1, Line(2, 1)),
  :properties => [1.0e5;],
  :x_coords => linspace(0, 1, 5),
  :support => [(5, [0])],
  :loaded_nodes => [(1,[-0.625]),(2,[-1.25]),(3,[-1.25]),(4,[-1.25]),(5,[-0.625])]
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
