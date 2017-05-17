"""
## formnf!

Returns nodal freedom numbering array nf

### Function
```julia
formnf!(nodof, nn, nf)
```

### Arguments
```julia
* nodof::Int       : Number of degrees of freedom for each node
* nn::Int          : Number of nodes in mesh
* nf::Array{Int,2} : Nodal freedom matrix (updated)
```
"""
function formnf!(nodof::Int, nn::Int, nf::Matrix{Int})
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
