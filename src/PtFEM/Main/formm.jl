"""
## formm!

This subroutine forms the derivatives of the invariants with respect to
stress in 2- or 3-d. See equation 6.25.

### Function
```julia
formm!(stress, m1, m2, m3)
```

### Arguments
```julia
* stress::Vector{Float64}    : Stress vector, see eq 6.25
* m1::Matrix{Float64}        : m1 matrix
* m2::Matrix{Float64}        : m2 matrix
* m3::Matrix{Float64}        : m3 matrix
```

### Return values
```julia
* m1::Matrix{Float64}        : m1 matrix
* m2::Matrix{Float64}        : m2 matrix
* m3::Matrix{Float64}        : m3 matrix
```
"""
function formm!(stress::Vector{Float64}, m1::Matrix{Float64}, 
  m2::Matrix{Float64}, m3::Matrix{Float64})
  #
  # This subroutine forms the derivatives of the invariants with respect to
  # stress in 2- or 3-d.
  #
  # Updates: m1, m2, m3
 nst = size(stress, 1)
 if nst == 4
   sx = stress[1]
   sy = stress[2]
   txy = stress[3]
   sz = stress[4]
   dx = (2.0*sx-sy-sz)/3.0
   dy = (2.0*sy-sz-sx)/3.0
   dz = (2.0*sz-sx-sy)/3.0
   sigm = (sx+sy+sz)/3.0
   m1[1,1:2] .= 1.0
   m1[2,1:2] .= 1.0
   m1[4,1:2] .= 1.0
   m1[1,4] = 1.0
   m1[4,4] = 1.0
   m1[2,4] = 1.0
   m1[:,:] = m1[:,:]/9.0/sigm
   m2[1,1] = 2.0/3.0
   m2[2,2] = 2.0/3.0
   m2[4,4] =  2.0/3.0
   m2[2,4] = -1.0/3.0
   m2[4,2] = -1.0/3.0
   m2[1,2] = -1.0/3.0
   m2[2,1] = -1.0/3.0
   m2[1,4] = -1.0/3.0
   m2[4,1] = -1.0/3.0
   m2[3,3] = 2.0
   m3[3,3] = -dz
   m3[1:2,3] .= txy/3.0
   m3[3,1:2] .= txy/3.0
   m3[3,4] = -2.0*txy/3.0
   m3[4,3] = -2.0*txy/3.0
   m3[1,1] = dx/3.0
   m3[2,4] = dx/3.0
   m3[4,2] = dx/3.0
   m3[2,2] = dy/3.0
   m3[1,4] = dy/3.0
   m3[4,1] = dy/3.0
   m3[4,4] = dz/3.0
   m3[1,2] = dz/3.0
   m3[2,1] = dz/3.0
 elseif nst == 6
   sx = stress[1]
   sy = stress[2]    
   sz = stress[3]
   txy = stress[4]  
   tyz = stress[5] 
   tzx = stress[6]
   sigm = (sx+sy+sz)/3.0
   dx = sx-sigm  
   dy = sy-sigm 
   dz = sz-sigm
   m1[1:3,1:3] = 1.0/(3.0*sigm)
   for i in 1:3 
     m2[i,i] = 2.0 
     m2[i+3,i+3] = 6.0 
   end
   m2[1,2] = -1.0
   m2[1,3] = -1.0 
   m2[2,3] = -1.0
   m3[1,1] = dx
   m3[1,2] = dz 
   m3[1,3] = dy 
   m3[1,4] = txy  
   m3[1,5] = -2.0*tyz
   m3[1,6] = tzx 
   m3[2,2] = dy 
   m3[2,3] = dx 
   m3[2,4] = txy
   m3[2,5] = tyz 
   m3[2,6] = -2.0*tzx 
   m3[3,3] = dz
   m3[3,4] = -2.0*txy
   m3[3,5] = tyz 
   m3[3,6] = tzx
   m3[4,4] = -3.0*dz 
   m3[4,5] = 3.0*tzx
   m3[4,6] = 3.0*tyz
   m3[5,5] = -3.0*dx
   m3[5,6] = 3.0*txy 
   m3[6,6] = -3.0*dy
   for i in 1:6 
     for j in i+1:6
       m1[j,i] = m1[i,j]
       m2[j,i] = m2[i,j]
       m3[j,i] = m3[i,j]
     end
   end
   m1[:,:] = m1/3.0
   m2[:,:] = m2/3.0
   m3[:,:] = m3/3.0
 else
   println("nst size not recognised in formm.")
 end
 (m1, m2, m3)
end
