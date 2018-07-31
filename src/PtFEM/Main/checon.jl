"""
## checon

This subroutine sets converged to .FALSE. if relative change in loads
and oldlds is greater than tol and updates oldlds.

### Method
```julia
checon(loads, oldlds, tol)
```

### Arguments
```julia
* loads::Vector{Float64}        : Displacements vector/OffsetArray
* oldlds::Vector{Float64}       : Previous displacement vector/OffsetArray
* tol::Float64                  : Convergence tolerance
```

### Return value
```julia
* ::Bool                        : Convergence achieved
```

"""
function checon(loads, oldlds, tol)
  #
  # This subroutine sets converged to .FALSE. if relative change in loads
  # and oldlds is greater than tol and updates oldlds.
  #
  # Updates oldlds
  converged = maximum(abs.(loads-oldlds))/maximum(abs.(loads)) <= tol
  oldlds[:] .= loads
  converged
end