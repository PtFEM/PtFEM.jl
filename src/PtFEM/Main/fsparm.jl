"""

## fsparm!

Function fsparm assembles fin_el matrices into a Julia sparse
global stiffness matrix.

### Method
```julia
fsparm!(gsm, g, km)
```

### Arguments
```julia
* gsm::SparseArrays{Float64, Float64}   : Sparse stiffnes matrix (Updated)
* g::Vector{Int}                      : Global coordinate vector.
* km::Matrix{Float64}                   : Stiffness matrix.
```
"""
function fsparm!(gsm, g, km)
  #println("g = $g")
  ndof = size(g, 1)
  #println("ndof = $ndof")
  for i in 1:ndof
    k = g[i]
    #println("g[$i] = k = $k")
    if k !== 0
      for j in 1:ndof
        #println("i = $i, j = $j, g[$j] = $(g[j])")
        if g[j] !== 0
          iw = k - g[j]
          #println("iw = $iw")
          if iw >= 0
            if g[i] == g[j]
              gsm[g[i], g[j]] += km[i, j]
            else
              gsm[g[i], g[j]] += km[i, j]
              gsm[g[j], g[i]] += km[i, j]
            end  
            #println("gsm[$(g[i]), $(g[j])] += $(km[i,j])")
            #full(gsm) |> display
            #println()
          end
        end
      end
    end
  end
end
