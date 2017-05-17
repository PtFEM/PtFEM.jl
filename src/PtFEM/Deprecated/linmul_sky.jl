"""

## linmul_sky!

This subroutine forms the product of symmetric matrix stored as
a skyline and a vector.

### Method
```julia
linmul_sky!(kv, disps, loads, kdiag)
```

### Arguments
```julia
* kv::Vector{Float64}       : Sparse stiffnes matrix (Skyline format)
* disps::Vector{Float64}    : Displacements
* loads::Vector{Float64}    : Loads vector (Updated)
* kdiag::Vector{Int}      : Bandwidth vector
```
"""
function linmul_sky!(kv::Vector{Float64}, disps::Vector{Float64}, 
  loads::Vector{Float64}, kdiag::Vector{Int})
  #
  # This subroutine forms the product of symmetric matrix stored as
  # a skyline and a vector.
  #
  # Updates loads[:]
  #
 n = size(kdiag, 1)
 for i in 1:n
   x = 0.0
   lup = kdiag[i]
   if i == 1
     low = lup
   end
   if i !== 1
     low = kdiag[i-1] + 1
   end
   for j in low:lup
     x += kv[j] * disps[i+j-lup] 
   end
   loads[i] = x
   i == 1 && continue  
   lup = lup - 1
   for j in low:lup
     k = i + j - lup - 1
     loads[k] += kv[j] * disps[i]        
   end
 end
 loads
end