using CSoM

F = 5.0       # Sum of equally distributed loads (1 N/m)
N = 5         # Number of nodes
els = N - 1   # Number of elements

data = Dict(
  # Rod(nels, np_types, nip, Line(nod, nodof))
  :struc_el => Rod(N-1, 1, 1, Line(1)),
  :properties => [1.0e5;],
  :x_coords => collect(linspace(0.0, 1.0, N)),
  :support => [(N, [0])],
)

data[:loaded_nodes] = [(i, [0.0]) for i in 1:N]
for node in 1:N-1
  data[:loaded_nodes][node][2][1] += F / (2els)
  data[:loaded_nodes][node+1][2][1] += F / (2els)
end  

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

F = 10.0
data[:loaded_nodes] = [(i, [0.0]) for i in 1:N]
for node in 1:N-1
  data[:loaded_nodes][node][2][1] += F / (2els)
  data[:loaded_nodes][node+1][2][1] += F / (2els)
end  

@time m2 = FE4_1(m, data)
println()

println("Displacements:")
m2.displacements |> display
println()

println("Actions:")
m2.actions |> display
println()

