"""
## beam_mm

This subroutine forms the stiffness matrix of a beam element (bending only).

### Method
```julia
beam_mm(ei, ell)
```

### Arguments
```julia
* fs::Float64               : Element density
* ell::Float64              : Element length
```

### Return value
```julia
* mm::::Matrix{Float64}     : Mass matrix for beam elembeam_mmated)
```
"""
function beam_mm(fs::Float64, ell::Float64)
  #
  # This subroutine forms the consistent mass matrix of a beam fin_el.
  #
  fac = (fs*ell)/420.0
  mm = zeros(4,4)
  mm[1,1] = 156.0*fac
  mm[3,3] = mm[1,1]
  mm[1,2] = 22.0*ell*fac
  mm[2,1] = mm[1,2]
  mm[3,4] = -mm[1,2]
  mm[4,3] = mm[3,4]
  mm[1,3] = 54.0*fac
  mm[3,1] = mm[1,3]
  mm[1,4] = -13.0*ell*fac
  mm[4,1] = mm[1,4]
  mm[2,3] = -mm[1,4]
  mm[3,2] = mm[2,3]
  mm[2,2] = 4.0*(ell*ell)*fac
  mm[4,4] = mm[2,2]
  mm[2,4] = -3.0*(ell*ell)*fac
  mm[4,2] = mm[2,4]
  mm
end