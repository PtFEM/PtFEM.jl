"""
## rod_mm!

This subroutine forms the consistent mass matrix of a 1-d "rod" fin_el.

### Function
```julia
rod_mm!(km, ell)
```

### Arguments
```julia
* mm::Matrix{Float64}       : Element mass matrix (Updated)
* ell::Float64              : Element length
```
"""
function rod_mm!(mm::Matrix{Float64}, ell::Float64)
  #
  # This subroutine forms the consistent mass matrix of a 1-d "rod" fin_el.
  #
 mm = [1.0/3.0 1.0/6.0; 1.0/6.0 1.0/3.0]
 mm *= ell
end