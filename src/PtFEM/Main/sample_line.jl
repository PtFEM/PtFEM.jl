"""
## sample!

This subroutine returns the local coordinates and weighting coefficients
of the integrating points.

### Function
```julia
sample!(fin_el, s, wt)
```

### Arguments
```julia
* fin_el::FiniteElement      : Finite element type
* s::Matrix{Float64}        : Local coordinates (Updated)
* wt::Vector{Float64}       : Weighting coefficients (Updated)
```
"""
function sample!(fin_el::Line, s::Matrix{Float64} , wt::Vector{Float64})
  #
  # This subroutine returns the local coordinates and weighting coefficients
  # of the integrating points.
  #
  nip = size(s,1)
  (s[:,1], wt[:]) = QuadGK.gauss(Float64, nip)
end
