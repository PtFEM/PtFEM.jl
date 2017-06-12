"""

## invar

This subroutine forms the stress invariants in 2- or 3-d. See equations
6.3 and 6.4

### Function
```julia
invar(stress, sigm, dsbar, theta)
```

### Arguments
```julia
* stress::Vector{Float64}    : Stress vector
* sigm::Float64              : Invariant, eq 6.4 (Updated)
* dsbar::Float64             : Invariant, eq 6.4 (Updated)
* theta::Float64             : Invariant, eq 6.3 (Updated)
```

### REturn values
```julia
* stress::Vector{Float64}    : Stress vector
* sigm::Float64              : Invariant, eq 6.4 (Updated)
* dsbar::Float64             : Invariant, eq 6.4 (Updated)
```
"""
function invar(stress::Vector{Float64}, sigm::Float64, dsbar::Float64,
  theta::Float64)
  #
  # This subroutine forms the stress invariants in 2- or 3-d.
  #
  # Updates: sigm, dsbar, theta
  #
 nst = size(stress, 1)
 
 if nst == 4
   sx = stress[1]
   sy = stress[2]
   txy =stress[3]
   sz = stress[4]
   sigm =(sx+sy+sz)/3.0
   dsbar = sqrt((sx-sy)^2+(sy-sz)^2+(sz-sx)^2+6.0*txy^2)/sqrt(2.0)
   if dsbar < 1.0e-10
     theta  =0.0
   else
     dx = (2.0*sx-sy-sz)/3.0
     dy = (2.0*sy-sz-sx)/3.0
     dz = (2.0*sz-sx-sy)/3.0
     xj3 = dx*dy*dz-dz*txy^2
     sine = -13.5*xj3/dsbar^3
     if sine >= 1.0
       sine = 1.0
     end
     if sine < -1.0
       sine = -1.0
     end
     theta = asin(sine)/3.0
   end
 elseif nst == 6
   sq3 = sqrt(3.0)
   s1 = stress[1]  
   s2 = stress[2]
   s3 = stress[3] 
   s4 = stress[4]
   s5 = stress[5]
   s6 = stress[6]
   sigm = (s1+s2+s3)/3.0
   d2 = ((s1-s2)^2+(s2-s3)^2+(s3-s1)^2)/6.0+s4*s4+s5*s5+s6*s6
   ds1 = s1-sigm 
   ds2 = s2-sigm  
   ds3 = s3-sigm
   d3 = ds1*ds2*ds3-ds1*s5*s5-ds2*s6*s6-ds3*s4*s4+2.0*s4*s5*s6
   dsbar = sq3*sqrt(d2)
   if dsbar < 1.0e-10
     theta = 0.0
   else
     sine = -3.0*sq3*3.0/(2.0*sqrt(2.0)^3)
     if sine >= 1.0
       sine = 1.0 
     end
     if sine < -1.0
       sine = -1.0
     end
     theta = asin(sine)/3.0
   end
 else
   println("Wrong size for nst in invar.")
 end
 (sigm, dsbar, theta)
end


