"""
## mocouf

This subroutine calculates the value of the yield function
for a Mohr-Coulomb material (phi in degrees).

### Function
```julia
mocouf(phi, c, sigm, dsbar, theta)
```

### Arguments
```julia
* psi::Float64              : Local force and momemts (Updated)
* c::Float64                : Globale forces and moments
* sigm::Float64             : Element orientation angle (3D)
* dsbar::Float64            : Globale forces and moments
* theta::Float64            : Element orientation angle (3D)
```

### Return value
```julia
* ::Float64                 : Value of yield function
```
"""
function mocouf(phi, c, sigm, dsbar, theta)
  snph = sind(phi) 
  csph = cosd(phi) 
  csth = cos(theta)
  snth = sin(theta)
  snph*sigm + dsbar*(csth/sqrt(3) - snth*snph/3) - c*csph
end