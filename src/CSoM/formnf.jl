"""
## formnf!

Returns nodal freedom array nf

### Function
```julia
formnf!(nodof::Int64, nn::Int64, nf::Matrix{Int64})
```

### Arguments
```julia
* nodof::Int64       : Number of degrees of freedom for each node
* nn::Int64          : Number of nodes in mesh
* nf::Array{Int64,2} : Nodal freedom matrix (updated)
```
"""
function formnf!(nodof::Int64, nn::Int64, nf::Matrix{Int64})
  local m = 0
  for j in 1:nn
    for i in 1:nodof
      if nf[i,j] != 0
        m += 1
        nf[i,j] = m
      end
    end
  end
end
