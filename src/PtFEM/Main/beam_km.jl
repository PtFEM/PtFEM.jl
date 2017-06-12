"""
## beam_km

This subroutine forms the stiffness matrix of a beam element (bending only).

### Method
```julia
beam_km(ei, ell)
```

### Arguments
```julia
* ei::Float64               : Element stiffness
* ell::Float64              : Element length
```
### Return value
```julia
* km::::Matrix{Float64}     : Stiiness matrix for beam element (Updated)
```
"""
function beam_km(ei::Float64, ell::Float64)
  #
  # This subroutine forms the stiffness matrix of a
  # beam fin_el (bending only).
  #
  km = zeros(4,4)
  km[1,1] = 12.0*ei/(ell*ell*ell) 
  km[3,3] = km[1,1]
  km[1,2] = 6.0*ei/(ell*ell) 
  km[2,1] = km[1,2] 
  km[1,4] = km[1,2]
  km[4,1] = km[1,4] 
  km[1,3] = -km[1,1] 
  km[3,1] = km[1,3] 
  km[3,4] = -km[1,2]
  km[4,3] = km[3,4] 
  km[2,3] = km[3,4] 
  km[3,2] = km[2,3]
  km[2,2] = 4.0*ei/ell
  km[4,4] = km[2,2] 
  km[2,4] = 2.0*ei/ell 
  km[4,2] = km[2,4]
  km
end