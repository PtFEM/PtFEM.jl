using PtFEM

old = pwd()
ProjDir = joinpath(dirname(@__FILE__), "..", "examples", "4 Static Equilibrium")
#cd(ProjDir)

include(joinpath(ProjDir, "p4.1.1.jl"))

#=
data = Dict(
  # Rod(nels, np_types, nip, finite_element(nod, nodof))
  :struc_el => Rod(4, 1, 1, Line(2, 1)),
  :properties => [1.0e5;],
  :x_coords => range(0, stop=1, length=5),
  :support => [(1, [0])],
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
=#

@show m.loads
println()

m1 = fromSkyline(m.kv, m.kdiag)
@show m1
println()

@show m.kdiag
println()

@show m.kv
println()

ms = skyline2sparse(m.kv, m.kdiag)
@show ms
println()

loads = zeros(m.neq+1)
if :loaded_nodes in keys(data)
  for i in 1:size(data[:loaded_nodes], 1)
    loads[m.nf[:, data[:loaded_nodes][i][1]]+1] = data[:loaded_nodes][i][2]
  end
end

@show loads
println()

@show m1 \ loads[2:end]
println()

@show ms \ loads[2:end]
println()
cd(old)
