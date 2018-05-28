"""
## shape_fun!

This subroutine produces derivatives of shape functions with respect
to local coordinates.

### Function
```julia
shape_fun!(fun, point, i)
```

### Arguments
```julia
* fun::Vector{Float64}       : Shape function (Updated)
* points::Matrix{Float64}    : Local coordinates of integration points
* i::Int                   : Integration point
```
"""
function shape_fun!(fun::Vector{Float64}, points::Matrix{Float64}, i::Int)
  #
  #   This subroutine computes the values of the shape functions.
  #   to local coordinates
  #

  ndim = size(points,2)
  nod = length(fun)  
  if ndim == 1                                # 1.0 dimensional case
    xi=points[i,1]
    if nod == 1
      t1 = -1.0-xi 
      t2 = 1.0-xi
      fun[1] = t2/2.0 
      fun[2] = -t1/2.0
    elseif nod == 3
      t1=-1.0-xi 
      t2=-xi 
      t3=1.0-xi
      fun[1] = t2*t3/2.0 
      fun[2] = -t1*t3 
      fun[3] = t1*t2/2.0
    elseif nod == 4
      t1=-1.0-xi 
      t2=-1.0/3.0-xi 
      t3=1.0/3.0-xi 
      t4=1.0-xi
      fun[1] = t2*t3*t4*9.0/16.0  
      fun[2] = -t1*t3*t4*27.0/16.0
      fun[3] = t1*t2*t4*27.0/16.0 
      fun[4] = -t1*t2*t3*9.0/16.0
    elseif nod == 5
      t1=-1.0 -xi 
      t2=-0.5-xi 
      t3=-xi 
      t4=0.5-xi 
      t5=1.0-xi
      fun[1] = t2*t3*t4*t5*2.0/3.0 
      fun[2] = -t1*t3*t4*t5*8.0/3.0
      fun[3] = t1*t2*t4*t5*4.0 
      fun[4] = -t1*t2*t3*t5*8.0/3.0
      fun[5] = t1*t2*t3*t4*2.0/3.0
    else
      println("Wrong number of nodes in shape_fun.")
    end
  elseif ndim == 2                                # 2.0 dimensional case
    c1=points[i,1]
    c2=points[i,2]
    c3=1.0-c1-c2 
    xi=points[i,1]
    eta=points[i,2]
    etam=0.25*(1.0-eta)
    etap=0.25*(1.0+eta)
    xim=0.25*(1.0-xi)
    xip=0.25*(1.0+xi)
    if nod == 3
      fun[:] = [c1,c3,c2]
    elseif nod == 4
      fun[:] = [4.0*xim*etam,4.0*xim*etap,4.0*xip*etap,4.0*xip*etam]
    elseif nod == 5
      fun[:] = [4.0*xim*etam-0.25*(1.0-xi*xi)*(1.0-eta*eta),
            4.0*xim*etap-0.25*(1.0-xi*xi)*(1.0-eta*eta),
            4.0*xip*etap-0.25*(1.0-xi*xi)*(1.0-eta*eta),
            4.0*xip*etam-0.25*(1.0-xi*xi)*(1.0-eta*eta),
            (1.0-xi*xi)*(1.0-eta*eta)]
    elseif nod == 6
      fun[1] = (2.0*c1-1.0)*c1 
      fun[2] = 4.0*c3*c1
      fun[3] = (2.0*c3-1.0)*c3 
      fun[4] = 4.0*c2*c3      
      fun[5] = (2.0*c2-1.0)*c2
      fun[6] = 4.0*c1*c2 
    elseif nod == 8
      fun[:] = [4.0*etam*xim*(-xi-eta-1.0),32.0*etam*xim*etap,
            4.0*etap*xim*(-xi+eta-1.0),32.0*xim*xip*etap,
            4.0*etap*xip*(xi+eta-1.0), 32.0*etap*xip*etam,
            4.0*xip*etam*(xi-eta-1.0), 32.0*xim*xip*etam]
    elseif nod == 9
      etam=eta-1.0
      etap=eta+1.0
      xim=xi-1.0
      xip=xi+1.0
      fun[:] = [0.25*xi*xim*eta*etam,-0.5*xi*xim*etap*etam,
            0.25*xi*xim*eta*etap,-0.5*xip*xim*eta*etap,
            0.25*xi*xip*eta*etap,-0.5*xi*xip*etap*etam,
            0.25*xi*xip*eta*etam,-0.5*xip*xim*eta*etam,
            xip*xim*etap*etam]
    elseif nod == 10
      fun[1] =  ((3.0*c1-1.0)*(3.0*c1-2.0)*c1)/2.0
      fun[2] =  -(9.0*(3.0*c1-1.0)*(c1+c2-1.0)*c1)/2.0
      fun[3] =   (9.0*(3.0*c1+3.0*c2-2.0)*(c1+c2-1.0)*c1)/2.0
      fun[4] = -((3.0*c1+3.0*c2-1.0)*(3.0*c1+3.0*c2-2.0)*(c1+c2-1.0))/2.0    
      fun[5] =   (9.0*(3.0*c1+3.0*c2-2.0)*(c1+c2-1.0)*c2)/2.0
      fun[6] =  -(9.0*(c1+c2-1.0)*(3.0*c2-1.0)*c2)/2.0
      fun[7] =  ((3.0*c2-1.0)*(3.0*c2-2.0)*c2)/2.0
      fun[8] =   (9.0*(3.0*c2-1.0)*c1*c2)/2.0
      fun[9] =   (9.0*(3.0*c1-1.0)*c1*c2)/2.0
      fun[10] = -27.0*((c2-1.0)+c1)*c1*c2
    elseif nod == 15
      t1=c1-0.25  
      t2=c1-0.5 
      t3=c1-0.75   
      t4=c2-0.25
      t5=c2-0.5   
      t6=c2-0.75 
      t7=c3-0.25  
      t8=c3-0.5 
      t9=c3-0.75
      fun[1] = 32.0/3.0*c1*t1*t2*t3   
      fun[2] = 128.0/3.0*c3*c1*t1*t2
      fun[3] = 64.0*c3*c1*t1*t7      
      fun[4] = 128.0/3.0*c3*c1*t7*t8
      fun[5] = 32.0/3.0*c3*t7*t8*t9   
      fun[6] = 128.0/3.0*c2*c3*t7*t8
      fun[7] = 64.0*c2*c3*t4*t7      
      fun[8] = 128.0/3.0*c2*c3*t4*t5
      fun[9] = 32.0/3.0*c2*t4*t5*t6   
      fun[10] = 128.0/3.0*c1*c2*t4*t5
      fun[11] = 64.0*c1*c2*t1*t4     
      fun[12] = 128.0/3.0*c1*c2*t1*t2
      fun[13] = 128.0*c1*c2*t1*c3    
      fun[15] = 128.0*c1*c2*c3*t4
      fun[14] = 128.0*c1*c2*c3*t7      
    else
      println("Wrong number of nodes in shape_fun.")
    end
  elseif ndim == 3                            # 3.0 dimensional case
    xi=points[i,1]
    eta=points[i,2]
    zeta=points[i,3]
    etam=1.0-eta 
    xim=1.0-xi  
    zetam=1.0-zeta
    etap=eta+1.0 
    xip=xi+1.0   
    zetap=zeta+1.0
    if nod == 4
     fun[1] = xi   
     fun[2] = eta 
     fun[3] = zeta 
     fun[4] = 1.0-fun[1]-fun[2]-fun[3]
    elseif nod == 8
     fun[:] = [0.125*xim*etam*zetam,0.125*xim*etam*zetap,
            0.125*xip*etam*zetap,0.125*xip*etam*zetam,
            0.125*xim*etap*zetam,0.125*xim*etap*zetap,
            0.125*xip*etap*zetap,0.125*xip*etap*zetam]
    elseif nod == 14                          # type 6 fin_el
     fun[1] = (xi*eta+xi*zeta+2.0*xi+eta*zeta+2.0*eta+2.0*zeta+2.0)*
       (xi-1.0)*(eta-1.0)*(zeta-1.0)/8.0
     fun[2] =-(xi*eta-xi*zeta+2.0*xi-eta*zeta+2.0*eta-2.0*zeta+2.0)*
       (xi-1.0)*(eta-1.0)*(zeta+1.0)/8.0
     fun[3] =-(xi*eta-xi*zeta+2.0*xi+eta*zeta-2.0*eta+2.0*zeta-2.0)*
       (xi+1.0)*(eta-1.0)*(zeta+1.0)/8.0
     fun[4] = (xi*eta+xi*zeta+2.0*xi-eta*zeta-2.0*eta-2.0*zeta-2.0)*
       (xi+1.0)*(eta-1.0)*(zeta-1.0)/8.0
     fun[5] =-(xi+1.0)*(xi-1.0)*(eta-1.0)*(zeta+1.0)*(zeta-1.0)/2.0
     fun[6] =-(xi-1.0)*(eta+1.0)*(eta-1.0)*(zeta+1.0)*(zeta-1.0)/2.0
     fun[7] = (xi+1.0)*(xi-1.0)*(eta+1.0)*(eta-1.0)*(zeta+1.0)/2.0
     fun[8] = (xi+1.0)*(eta+1.0)*(eta-1.0)*(zeta+1.0)*(zeta-1.0)/2.0
     fun[9] =-(xi+1.0)*(xi-1.0)*(eta+1.0)*(eta-1.0)*(zeta-1.0)/2.0
     fun[10] =  (xi*eta-xi*zeta-2.0*xi+eta*zeta+2.0*eta-2.0*zeta-2.0)*
       (xi-1.0)*(eta+1.0)*(zeta-1.0)/8.0
     fun[11] = -(xi*eta+xi*zeta-2.0*xi-eta*zeta+2.0*eta+2.0*zeta-2.0)*
       (xi-1.0)*(eta+1.0)*(zeta+1.0)/8.0
     fun[12] = -(xi*eta+xi*zeta-2.0*xi+eta*zeta-2.0*eta-2.0*zeta+2.0)*
       (xi+1.0)*(eta+1.0)*(zeta+1.0)/8.0
     fun[13] =  (xi*eta-xi*zeta-2.0*xi-eta*zeta-2.0*eta+2.0*zeta+2.0)*
       (xi+1.0)*(eta+1.0)*(zeta-1.0)/8.0
     fun[14] =  (xi+1.0)*(xi-1.0)*(eta+1.0)*(zeta+1.0)*(zeta-1.0)/2.0
    elseif nod == 20
      xii = [-1,-1,-1,0,1,1,1,0,-1,-1,1,1,-1,-1,-1,0,1,1,1,0]
      etai = [-1,-1,-1,-1,-1,-1,-1,-1,0,0,0,0,1,1,1,1,1,1,1,1]
      zetai = [-1,0,1,1,1,0,-1,-1,-1,1,1,-1,-1,0,1,1,1,0,-1,-1]
      for l in 1:20
        xi0 = xi*xii[l]
        eta0 = eta*etai[l]
        zeta0 = zeta*zetai[l]
        if l == 4 || l == 8 || l == 16 || l == 20
          fun[l] = 0.25*(1.0-xi*xi)*(1.0+eta0)*(1.0+zeta0)
        elseif l >= 9 && l <= 12
          fun[l] = 0.25*(1.0+xi0)*(1.0-eta*eta)*(1.0+zeta0)
        elseif l == 2 || l == 6 || l == 14 || l == 18
          fun[l] = 0.25*(1.0+xi0)*(1.0+eta0)*(1.0-zeta*zeta)
        else
          fun[l] = 0.125*(1.0+xi0)*(1.0+eta0)*(1.0+zeta0)*(xi0+eta0+zeta0-2)
        end
      end
    else
      println("Wrong number of nodes in shape_fun.")
    end
  else
   println("Wrong number of dimensions in shape_fun.")
  end
end