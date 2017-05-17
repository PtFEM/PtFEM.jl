"""
## fmplat!

This subroutine forms the 2nd derivatives for rectangular
plate bending fin_els.

### Method
```julia
fmplat!(d2x, d2y, d2xy, points, aa, bb, i)
```

### Arguments
```julia
* d2x::Vector{Float64}       : x derivative term (Updated)
* d2y::Vector{Float64}       : y derivative term (Updated)
* d2xy::Vector{Float64}      : x,y derivative term (Updated)
* points::Matrix{Float64}    : Location of Gauss points
* aa::Float64                : Dimension of plate
* bb::Float64                : Dimension of plate
* i::Int                   : Gauss point to use
```
"""
function fmplat!(d2x::Vector{Float64}, d2y::Vector{Float64}, d2xy::Vector{Float64}, 
  points::Matrix{Float64}, aa::Float64, bb::Float64, i::Int)
  #
  # This subroutine forms the 2nd derivatives for rectangular
  # plate bending fin_els.
  #
  # Updates d2x, d2y and d2xy vectors
  #
 x = points[i,1]
 e = points[i,2]
 xp1 = x+1.0
 xp12 = xp1*xp1
 xp13 = xp12*xp1
 ep1 = e+1.0
 ep12 = ep1*ep1
 ep13 = ep12*ep1
 p1 = 1.0-0.75*xp12+0.25*xp13
 q1 = 1.0-0.75*ep12+0.25*ep13
 p2 = 0.5*aa*xp1*(1.0-xp1+0.25*xp12)
 q2 = 0.5*bb*ep1*(1.0-ep1+0.25*ep12)
 p3 = 0.25*xp12*(3.0-xp1)
 q3 = 0.25*ep12*(3.0-ep1)
 p4 = 0.25*aa*xp12*(0.5*xp1-1.0)
 q4 = 0.25*bb*ep12*(0.5*ep1-1.0)
 dp1 = 1.5*xp1*(0.5*xp1-1.0)
 dq1 = 1.5*ep1*(0.5*ep1-1.0)
 dp2 = aa*(0.5-xp1+0.375*xp12)
 dq2 = bb*(0.5-ep1+0.375*ep12)
 dp3 = 1.5*xp1*(1.0-0.5*xp1)
 dq3 = 1.5*ep1*(1.0-0.5*ep1)
 dp4 = 0.5*aa*xp1*(0.75*xp1-1.0)
 dq4 = 0.5*bb*ep1*(0.75*ep1-1.0)
 d2p1 = 1.5*x
 d2p2 = 0.25*aa*(3.0*x-1.0)
 d2p3 = -d2p1
 d2p4 = 0.25*aa*(3.0*x+1.0)
 d2q1 = 1.5*e
 d2q2 = 0.25*bb*(3.0*e-1.0)
 d2q3 = -d2q1
 d2q4 = 0.25*bb*(3.0*e+1.0)
 d2x[1] = d2p1*q1
 d2x[2] = d2p2*q1
 d2x[3] = d2p1*q2
 d2x[4] = d2p2*q2
 d2x[5] = d2p1*q3
 d2x[6] = d2p2*q3
 d2x[7] = d2p1*q4
 d2x[8] = d2p2*q4
 d2x[9] = d2p3*q3
 d2x[10] = d2p4*q3
 d2x[11] = d2p3*q4
 d2x[12] = d2p4*q4
 d2x[13] = d2p3*q1
 d2x[14] = d2p4*q1
 d2x[15] = d2p3*q2
 d2x[16] = d2p4*q2
 d2y[1] = p1*d2q1
 d2y[2] = p2*d2q1
 d2y[3] = p1*d2q2
 d2y[4] = p2*d2q2
 d2y[5] = p1*d2q3
 d2y[6] = p2*d2q3
 d2y[7] = p1*d2q4
 d2y[8] = p2*d2q4
 d2y[9] = p3*d2q3
 d2y[10] = p4*d2q3
 d2y[11] = p3*d2q4
 d2y[12] = p4*d2q4
 d2y[13] = p3*d2q1
 d2y[14] = p4*d2q1
 d2y[15] = p3*d2q2
 d2y[16] = p4*d2q2
 d2xy[1] = dp1*dq1
 d2xy[2] = dp2*dq1
 d2xy[3] = dp1*dq2
 d2xy[4] = dp2*dq2
 d2xy[5] = dp1*dq3
 d2xy[6] = dp2*dq3
 d2xy[7] = dp1*dq4
 d2xy[8] = dp2*dq4
 d2xy[9] = dp3*dq3
 d2xy[10] = dp4*dq3
 d2xy[11] = dp3*dq4
 d2xy[12] = dp4*dq4
 d2xy[13] = dp3*dq1
 d2xy[14] = dp4*dq1
 d2xy[15] = dp3*dq2
 d2xy[16] = dp4*dq2
 #(d2x, d2y, d2xy)
end