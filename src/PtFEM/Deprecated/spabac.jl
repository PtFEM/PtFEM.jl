"""

## spabac!

Function spabac! performs Cholesky forward and back-substitution on
a symmetric skyline global matrix. The loads vector is updated.

###Arguments
```julia
spabac!(kv, loads, kdiag)
```

### Arguments
```julia
* kv::Vector{Float64}       : Skyline vector of global stiffness matrix
* loads::Vector{Float64}    : Load vector (Updated)
* kdiag::Vector{Int}      : Diagonal elemnt index vector
```
"""
function spabac!(kv::Vector{Float64}, loads::Vector{Float64}, kdiag::Vector{Int})
  local x::Float64
  n = size(kdiag, 1)
  loads[1]=loads[1]/kv[1]
  for i in 2:n
    ki = kdiag[i]-i
    l = kdiag[i-1] - ki + 1
    x = loads[i]
    if l !== i
      m = i - 1
      for j in l:m
        x -= kv[ki+j]*loads[j]
      end
    end
    loads[i] = x/kv[ki+i]
  end
  for it in 2:n
    i = n + 2 - it
    ki = kdiag[i] - i
    x = loads[i]/kv[ki+i]
    loads[i] = x
    l = kdiag[i-1] - ki + 1
    if l !== i
      m = i - 1
      for k in l:m
        loads[k] -= x*kv[ki+k]
      end
    end
  end
  loads[1] /= kv[1]
  loads
end

