"""
## loc_to_glob!

This subroutine transforms the local end reactions and
moments into the global system (3-d).

### Function
```julia
loc_to_glob!(loc, glob, gamma, coord)
```

### Arguments
```julia
* loc::Vector{Float64}       : Local force and momemts (Updated)
* glob::Vector{Float64}      : Globale forces and moments
* gamma::Float64             : Element orientation angle (3D)
* coord::Matrix{Float64}     : Nodal coordinates
```
"""
function loc_to_glob!(loc::Vector{Float64}, glob::Vector{Float64}, 
  gamma::Float64, coord::LinearAlgebra.Adjoint{Float64,Array{Float64,2}})
  #
  # This subroutine transforms the local end reactions and
  # moments into the global system (3-d).
  #
  # Updates glob[:]
  #
  t = zeros(12, 12)
  r0 = zeros(3, 3)

  x1 = coord[1,1]
  y1 = coord[1,2]
  z1 = coord[1,3]
  x2 = coord[2,1]
  y2 = coord[2,2]
  z2 = coord[2,3]
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
   r0[2,1] = yl/ell
   r0[3,1] = zl/ell
   r0[1,2] = (-xl*yl*cg-ell*zl*sg)/den
   r0[2,2] = den*cg/(ell*ell)
   r0[3,2] = (-yl*zl*cg+ell*xl*sg)/den
   r0[1,3] = (xl*yl*sg-ell*zl*cg)/den
   r0[2,3] = -den*sg/(ell*ell)
   r0[3,3] = (yl*zl*sg+ell*xl*cg)/den
  else
   r0[1,1] = 0.0
   r0[3,1] = 0.0
   r0[2,2] = 0.0
   r0[2,3] = 0.0
   r0[2,1] = 1.0
   r0[1,2] = -cg
   r0[3,3] = cg
   r0[3,2] = sg
   r0[1,3] = sg
  end
  for i in 1:3
   for j in 1:3 
     x = r0[i,j]
     for k in 0:3:9
       t[i+k, j+k] = x
     end
   end
  end
  for i in 1:12
   sum = 0.0
   for j in 1:12
     sum = sum+t[i,j]*loc[j]
   end
   glob[i] = sum
  end
  glob
end