"""

## fsparv!

Function fsparv! assembles fin_el matrices into a symmetric skyline
global matrix. The Skyline vector kv is updated.

### Method
```julia
fsparv!(kv, km, g, km)
```

### Arguments
```julia
* kv::Vector{Float64}        : Sparse stiffnes matrix (Updated)
* km::Matrix{Float64}        : Symmetric element stiffnes matrix
* g::Vector{Int}           : Global steering vector.
* kdiag::Vector{Int}       : Location of diagoinal terms
```
"""
function fsparv!(kv::Vector{Float64}, km::Matrix{Float64},
  g::Vector{Int}, kdiag::Vector{Int})
  ndof = size(g, 1)
  for i in 1:ndof
    k = g[i]
    if k !== 0
      for j in 1:ndof
        if g[j] !== 0
          iw = k - g[j]
          if iw >= 0
            ival = kdiag[k] - iw
            kv[ival] += km[i, j]
          end
        end
      end
    end
  end
end

