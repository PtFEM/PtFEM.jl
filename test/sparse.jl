using SparseVectors, CSoM

include("/Users/rob/.julia/v0.4/CSoM/examples/StaticEquilibrium/p4.1.1.jl")

m1 = fromSkyline(m.kv, m.kdiag)
m1 |> display
println()

m.kdiag |> display
println()

m.kv |> display
println()

sparse(m1)

