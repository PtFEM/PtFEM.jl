"""
## num_to_g!

Returns the element steering vector g from the element node numbering num
and the nodal freedom array nf.

### Function
```julia
num_to_g!(num, nf, g)
```

### Arguments
```julia
* num::Vector{Int64}       : Node numbering vector
* nf::Matrix{Int64}        : Nodal freedom array
* g::Vector{Int64}         : Element steering vector (Updated)
```
"""
function num_to_g!(num::Vector{Int64}, nf::Matrix{Int64}, g::Vector{Int64})
  nodof=size(nf,1)
  for i in 1:size(num, 1)
    k = i*nodof
    g[k-nodof+1:k] = nf[:, num[i]]
  end
  #g
end