using SparseVectors, CSoM

include("/Users/rob/.julia/v0.4/CSoM/examples/StaticEquilibrium/p4.1.1.jl")

@show m.loads
println()

m1 = fromSkyline(m.kv, m.kdiag)
@show m1
println()

@show m.kdiag
println()

@show m.kv
println()

sparse(m1)

loads = zeros(m.neq+1)
if :loaded_nodes in keys(data)
  for i in 1:size(data[:loaded_nodes], 1)
    loads[m.nf[:, data[:loaded_nodes][i][1]]+1] = data[:loaded_nodes][i][2]
  end
end

@show loads
println()

m1 \ loads[2:end]

A = [1.0 3;2.0 4]
b = [2, 5.0]
@show b
println()

c = A * b
@show c
println()

@show A \ c
println()
