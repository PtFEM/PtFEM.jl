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
* num::Vector{Int}       : Node numbering vector
* nf::Matrix{Int}        : Nodal freedom array
* g::Vector{Int}         : Element steering vector (Updated)
```
"""
function num_to_g!(num::Vector{Int}, nf::Matrix{Int}, g::Vector{Int})
  nodof=size(nf,1)
  for i in 1:size(num, 1)
    k = i*nodof
    g[k-nodof+1:k] = nf[:, num[i]]
  end
  #g
end