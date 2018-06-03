"""
## glob_to_loc!

This subroutine transforms the global end reactions and
moments into the local system (2- or 3-d). Called from hinge!().

### Function
```julia
glob_to_loc!(loc, glob, gamma, coord)
```

### Arguments
```julia
* loc::Vector{Float64}       : Local force and momemts (Updated)
* glob::Vector{Float64}      : Globale forces and moments
* gamma::Float64             : Element orientation angle (3D)
* coord::Matrix{Float64}     : Nodal coordinates
```
"""
function glob_to_loc!(loc::Vector{Float64}, glob::Vector{Float64},
  gamma::Float64, coord::LinearAlgebra.Adjoint{Float64,Array{Float64,2}})
  #
  # This subroutine transforms the global end reactions and
  # moments into the local system (2- or 3-d).
  #
  # Updates loc[:]
  #
  
  ndim = size(coord, 2)
  if ndim == 2
    x1 = coord[1, 1]
    y1 = coord[1, 2]
    x2 = coord[2, 1]
    y2 = coord[2, 2]
    ell = sqrt((x2-x1)^2+(y2-y1)^2)
    cg = (x2-x1)/ell
    sg = (y2-y1)/ell
    loc[1] = cg*glob[1]+sg*glob[2]
    loc[2] = cg*glob[2]-sg*glob[1]
    loc[3] = glob[3]
    loc[4] = cg*glob[4]+sg*glob[5]
    loc[5] = cg*glob[5]-sg*glob[4]
    loc[6] = glob[6]
  elseif ndim == 3
    t = zeros(12,12)
    r0 = zeros(3, 3)
    x1 = coord[1, 1]
    y1 = coord[1, 2]
    z1 = coord[1, 3]
    x2 = coord[2, 1]
    y2 = coord[2, 2]
    z2 = coord[2, 3]
    xl = x2 - x1
    yl = y2 - y1
    zl = z2 - z1
    ell = sqrt(xl*xl+yl*yl+zl*zl)
    gamrad = gamma*pi/180.0
    cg = cos(gamrad)
    sg = sin(gamrad)
    den = ell*sqrt(xl*xl+zl*zl)
    if den !== 0.0
      r0[1,1] = xl/ell
      r0[1,2] = yl/ell
      r0[1,3] = zl/ell
      r0[2,1] = (-xl*yl*cg-ell*zl*sg)/den
      r0[2,2] = den*cg/(ell*ell)
      r0[2,3] = (-yl*zl*cg+ell*xl*sg)/den
      r0[3,1] = (xl*yl*sg-ell*zl*cg)/den
      r0[3,2] = -den*sg/(ell*ell)
      r0[3,3] = (yl*zl*sg+ell*xl*cg)/den
    else
      r0[1,1] = 0.0
      r0[1,3] = 0.0
      r0[2,2] = 0.0
      r0[3,2] = 0.0
      r0[1,2] = 1.0
      r0[2,1] = -cg
      r0[3,3] = cg
      r0[2,3] = sg
      r0[3,1] = sg
    end
    for i in 1:3
      for j in 1:3 
        x = r0[i,j]
        for  k in 0:3:9
          t[i+k,j+k] = x
        end
      end
    end
    for i in 1:12
      sum = 0.0
      for j in 1:12
        sum = sum+t[i,j]*glob[j]
      end
      loc[i] = sum
    end
  end
end