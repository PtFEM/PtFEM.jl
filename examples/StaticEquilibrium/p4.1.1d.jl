using Compat, CSoM
include("FE4_1.jl")

N = 4
F = 5.0
dist_loads = [[(i, [-F/N]) for i in 1:(N+1)];]
dist_loads[1] = (1, [-F/(2*N)])
dist_loads[size(dist_loads,1)] = (N+1, [-F/(2*N)])
dist_loads = convert(Vector{Tuple{Int64, Vector{Float64}}}, dist_loads)

data = Dict(
  # Beam(ndim, nst, nxe, nip, direction, finite_element(nod, nodof), axisymmetric)
  :element_type => Beam(1, 1, N, 1, :x, Line(2, 1), false),
  :properties => [1.0e5;],
  :x_coords => linspace(0, 1, (N+1)),
  :support => [(1, [0])],
  :loaded_nodes => dist_loads
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
