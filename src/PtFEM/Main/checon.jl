"""
## checon!

This subroutine sets converged to .FALSE. if relative change in loads
and oldlds is greater than tol and updates oldlds.

### Method
```julia
checon!(loads, oldlds, tol)
```

### Arguments
```julia
* loads::Vector{Float64}        : Displacements vector
* oldlds::Vector{Float64}       : Previous displacement vector
* tol::Float64                  : Convergence tolerance
```
"""
function checon!(loads::Vector{Float64}, oldlds::Vector{Float64}, tol::Float64)
  #
  # This subroutine sets converged to .FALSE. if relative change in loads
  # and oldlds is greater than tol and updates oldlds.
  #
  # Updates oldlds
 converged = maximum(abs.(loads-oldlds))/maximum(abs.(loads)) <= tol
 oldlds[:] = loads
 converged
end