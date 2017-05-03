"""
## rod_km!

This subroutine forms the stiffness matrix of a 1-d "rod" fin_el.

### Function
```julia
rod_km!(km, ea, length)
```

### Arguments
```julia
* km::Matrix{Float64}       : Element stiffness matrix (Updated)
* ea::Float64               : Element stiffness
* ell::Float64              : Element length
```
"""
function rod_km!(km::Matrix{Float64}, ea::Float64, ell::Float64)
  #
  # This subroutine forms the stiffness matrix of a 1-d "rod" fin_el.
  #
 km = [1.0 -1.0;-1.0 1.0]
 km *= ea/ell
end