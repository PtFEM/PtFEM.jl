"""
## mocouq

This subroutine forms the derivatives of a Mohr-Coulomb potential
function with respect to the three invariants (psi in degrees).

### Function
```julia
(dq1,dq2,dq3) = mocouq(psi,dsbar,theta)
```

### Arguments
```julia
* psi::Float64               : Local force and momemts (Updated)
* dsbar::Float64             : Globale forces and moments
* theta::Float64             : Element orientation angle (3D)
```

### Return values
```julia
* dq1::Float64               : Local force and momemts (Updated)
* dq2::Float64               : Globale forces and moments
* dq3::Float64               : Element orientation angle (3D)
```
"""
function mocouq(psi,dsbar,theta)
 snth=sin(theta) 
 snps=sind(psi)
 sq3=sqrt(3.0)  
 dq1=snps
 if abs(snth) > 0.49
   c1 = 1.0
   snth < 0.0 && (c1 = -1.0)
   dq2 = (sq3*0.5-c1*snps*0.5/sq3)*sq3*0.5/dsbar 
   dq3 = 0.0
 else
   csth = cos(theta)
   cs3th = cos(3theta)
   tn3th = tan(3theta)
   tnth = snth/csth
   dq2 = sq3*csth/dsbar*((1.0+tnth*tn3th)+snps*(tn3th-tnth)/sq3)*0.5
   dq3 = 0.5*3.0*(sq3*snth+snps*csth)/(cs3th*dsbar*dsbar)
 end
 (dq1, dq2, dq3)
end