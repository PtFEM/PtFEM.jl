"""

## hinge!

This subroutine forms the end forces and moments to be
applied to a member if a joint has gone plastic.

### Function
```julia
hinge!(coord, holdr, action, react, prop, iel, etype, gamma)
```

### Arguments
```julia
* coord::Matrix{Float64}     : Nodal coordinates
* holdr::Matrix{Float64}     : Existing reactions
* action::Vector{Float64}    : Incremental reactions
* react::Vector{Float64}     : Correction to reactions (Updated)
* prop::Matrix{Float64}      : Beam properties
* iel::Int                 : Element number
* etype::Vector{Int}       : Element type
* gamma::Vector{Float64}     : Element orientation (3D)
```
"""
function hinge!(coord::LinearAlgebra.Adjoint{Float64,Array{Float64,2}}, holdr::Matrix{Float64}, 
  action::Vector{Float64}, react::Vector{Float64}, prop::Matrix{Float64}, 
  iel, etype::Vector{Int}, gamma::Vector{Float64})
  #
  # This subroutine forms the end forces and moments to be
  # applied to a member if a joint has gone plastic.
  #
  # Updates react
  #
 ndim = size(coord,2)
 ndof = size(action,1)
 
 glob = zeros(ndof)
 loc = zeros(ndof)
 total = zeros(ndof)
 
 bm1 = 0.0
 bm2 = 0.0
 bm3 = 0.0
 bm4 = 0.0
 
 total[:] = holdr[:,iel]
 if ndim == 1
   mpy = prop[etype[iel], 2]
   ell=coord[2,1]-coord[1,1]
   s1=total[2]+action[2]
   s2=total[4]+action[4]
   if abs(s1) > mpy
     if s1 > 0.0
       bm1 = mpy-s1
     end
     if s1 <= 0.0
       bm1 = -mpy-s1
     end
   end
   if abs(s2) > mpy
     if s2 > 0.0
       bm2 = mpy - s2
     end
     if s2 <= 0.0
       bm2 = -mpy - s2
     end
   end
   react[1] = (bm1+bm2)/ell
   react[2] = bm1
   react[3] = -react[1]
   react[4] = bm2
 elseif ndim == 2
   mpy = prop[etype[iel], 3]  
   x1 = coord[1,1]
   y1 = coord[1,2]
   x2 = coord[2,1]
   y2 = coord[2,2]
   ell = sqrt((y2-y1)^2+(x2-x1)^2)
   csch = (x2-x1)/ell
   snch = (y2-y1)/ell
   s1=total[3]+action[3]
   s2=total[6]+action[6]
   if abs(s1) > mpy
     if s1 > 0.0
       bm1 = mpy - s1
     end
     if s1 <= 0.0
       bm1 = -mpy - s1
     end
   end
   if abs(s2) > mpy
     if s2 > 0.0
       bm2 = mpy - s2
     end
     if s2 <= 0.0
       bm2 = -mpy - s2
     end
   end
   react[1] = -(bm1+bm2)*snch/ell
   react[2] =  (bm1+bm2)*csch/ell
   react[3] = bm1
   react[4] = -react[1]
   react[5] = -react[2]
   react[6] = bm2
 elseif ndim == 3
   gam = gamma[iel]
   mpy = prop[etype[iel], 5]
   mpz = prop[etype[iel], 6]
   mpx = prop[etype[iel], 7]
   x1 = coord[1,1]
   y1 = coord[1,2]
   z1 = coord[1,3]
   x2 = coord[2,1]
   y2 = coord[2,2]
   z2 = coord[2,3]
   ell = sqrt((z2-z1)^2+(y2-y1)^2+(x2-x1)^2)
   glob = total + action
   glob_to_loc!(loc,glob,gam,coord)
   glob = zeros(ndof)
   s1 = loc[5]
   s2 = loc[11]
   if abs(s1) > mpy
     if s1 > 0.0
       bm1 = mpy - s1
     end
     if s1 <= 0.0
       bm1 = -mpy - s1
     end
   end
   if abs(s2) > mpy
     if s2 > 0.0
       bm2 = mpy - s2
     end
     if s2 <= 0.0
       bm2 = -mpy - s2
     end
   end
   loc[ 3] = -(bm1+bm2)/ell
   loc[ 9] = -loc[3]
   loc[ 5] =  bm1
   loc[11] =  bm2
   s3=loc[6]
   s4=loc[12]
   if abs(s3) > mpz
     if s3 > 0.0
       bm1 = mpz - s3
     end
     if s3 <= 0.0
       bm1 = -mpz - s3
     end
   end
   if abs(s4) > mpy
     if s4 > 0.0
       bm2 = mpz - s4
     end
     if s4 <= 0.0
       bm2 = -mpz - s4
     end
   end
   loc[ 2] = (bm3+bm4)/ell
   loc[ 8] = -loc[2]
   loc[ 6] =  bm3
   loc[12] =  bm4
   s5=loc[4]
   if abs(s5) > mpx
     if s5 > 0.0
       glob[4] =  mpx - s5
     end
     if s5 <= 0.0
       glob[4] = -mpx - s5
     end
   end
   loc[10] = -loc[4]
   #@show loc
   loc_to_glob!(loc,react,gam,coord)
   #@show react
 end
end