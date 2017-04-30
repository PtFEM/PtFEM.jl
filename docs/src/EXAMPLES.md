# Examples

## Introduction



## Ex41.1.jl

```julia
using PtFEM

data = Dict(
  # Rod(nels, np_types, nip, finite_element(nod, nodof))
  :struc_el => Rod(4, 1, 1, Line(2, 1)),
  :properties => [1.0e5;],
  :x_coords => linspace(0, 1, 5),
  :support => [(1, [0])],
  :loaded_nodes => [(1,[-0.625]),(2,[-1.25]),(3,[-1.25]),(4,[-1.25]),(5,[-0.625])]
)

fem, dis_dt, fm_dt = p41(data)

println("Displacements:")
dis_dt |> display
println()

println("Actions:")
fm_dt |> display
println()

```

