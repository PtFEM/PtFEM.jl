"""
## beam_gm!

This subroutine forms the beam geometric matrix for stability analysis.

### Method
```julia
beam_gm!(gm::Matrix{Float64}, ell::Float64)
```

### Arguments
```julia
* gm::::Matrix{Float64}     : Geometric matrix for beam element (Updated)
* ell::Float64              : Element length
```
"""
function beam_gm!(gm::Matrix{Float64}, ell::Float64)
  #
  # This subroutine forms the beam geometric matrix for stability analysis.
  #
  # Updated gm
  #
 gm[1,1] = 1.2/ell
 gm[1,2] = 0.1
 gm[2,1] = 0.1
 gm[1,3] = -1.2/ell
 gm[3,1] = -1.2/ell
 gm[1,4] = 0.1
 gm[4,1] = 0.1
 gm[2,2] = 2.0*ell/15.0
 gm[2,3] = -0.1
 gm[3,2] = -0.1
 gm[2,4] = -ell/30.0
 gm[4,2] = -ell/30.0
 gm[3,3] = 1.2/ell
 gm[3,4] = -0.1
 gm[4,3] = -0.1
 gm[4,4] = 2.0*ell/15.0
 gm
end