using CSoM

data = Dict(
  # Rod(nels, np_types, nip, Line(nod, nodof))
  :struc_el => Rod(4, 1, 1, Line(1)),
  :properties => [1.0e5;],
  :x_coords => 0.0:0.25:1.0,
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

# Stiffness matrix
println("Stiffness matrix:")
fromSkyline(m.kv, m.kdiag)
println()

# Reuse back-end of FE4_1

data[:loaded_nodes] = [(1,[-1.25]),(2,[-2.5]),(3,[-2.5]),(4,[-2.5]),(5,[-1.25])]

@time m2 = FE4_1(m, data)
println()

println("Displacements:")
m2.displacements |> display
println()

println("Actions:")
m2.actions |> display
println()

