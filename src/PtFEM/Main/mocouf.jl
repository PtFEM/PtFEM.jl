"""
## mocouf!!

This subroutine calculates the value of the yield function
for a Mohr-Coulomb material (phi in degrees).

### Function
```julia
mocouf!(phi, c, sigm, dsbar, theta, f)
```

### Arguments
```julia
* psi::Float64              : Local force and momemts (Updated)
* c::Float64                : Globale forces and moments
* sigm::Float64             : Element orientation angle (3D)
* dsbar::Float64            : Globale forces and moments
* theta::Float64            : Element orientation angle (3D)
```

### Updated arguments
```julia
* f::Float64                : Value of yield function
```
"""
function mocouf!(phi, c, sigm, dsbar, theta, f)
 snph = sind(phi) 
 csph = cosd(phi) 
 csth = cos(theta)
 snth = sin(theta)
 f = snph*sigm+dsbar*(csth/sqrt(3.0)-snth*snph/3.0)-c*csph
end