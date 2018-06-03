"""
## shape_der!

This subroutine produces derivatives of shape functions with respect
to local coordinates.

### Function
```julia
shape_der!(der, point, i)
```

### Arguments
```julia
* der::Matrix{Float64}       : Function derivative (Updated)
* points::Matrix{Float64}    : Local coordinates of integration points
* i::Int                   : Integration point
```
"""
function shape_der!(der::Matrix{Float64}, points::Matrix{Float64}, i::Int)
  #
  #   This subroutine produces derivatives of shape functions with respect
  #   to local coordinates.
  #

  ndim = size(der, 1)
  nod = size(der, 2)

  if ndim == 1                                          # 1.0 dimensional fin_els
   xi = points[i, 1]
   if nod == 2
     der[1,1] = -0.5 
     der[1,2] =  0.5
   elseif nod == 3
     t1=-1.0-xi 
     t2=-xi  
     t3=1.0-xi
     der[1,1] = -(t3+t2)/2.0  
     der[1,2] = (t3+t1)    
     der[1,3] = -(t2+t1)/2.0   
   elseif nod == 4
     t1=-1.0-xi 
     t2=-1.0/3.0-xi 
     t3=1.0/3.0-xi 
     t4=1.0-xi
     der[1,1] = -(t3*t4+t2*t4+t2*t3)*9.0/16.0     
     der[1,2] = (t3*t4+t1*t4+t1*t3)*27.0/16.0 
     der[1,3] = -(t2*t4+t1*t4+t1*t2)*27.0/16.0 
     der[1,4] = (t2*t3+t1*t3+t1*t2)*9.0/16.0   
   elseif nod == 5
     t1=-1.0-xi 
     t2=-0.5-xi 
     t3=-xi 
     t4=0.5-xi 
     t5=1.0-xi
     der[1,1] = -(t3*t4*t5+t2*t4*t5+t2*t3*t5+t2*t3*t4)*2.0/3.0   
     der[1,2] = (t3*t4*t5+t1*t4*t5+t1*t3*t5+t1*t3*t4)*8.0/3.0
     der[1,3] = -(t2*t4*t5+t1*t4*t5+t1*t2*t5+t1*t2*t4)*4.0 
     der[1,4] = (t2*t3*t5+t1*t3*t5+t1*t2*t5+t1*t2*t3)*8.0/3.0
     der[1,5] = -(t2*t3*t4+t1*t3*t4+t1*t2*t4+t1*t2*t3)*2.0/3.0
   else
     println("Wrong number of nodes in shape_der.")
   end
  elseif ndim == 2                                      # 2.0 dimensional fin_els
   xi=points[i, 1]
   eta=points[i, 2] 
   c1=xi 
   c2=eta 
   c3=1.0-c1-c2
   etam=0.25*(1.0-eta)
   etap=0.25*(1.0+eta)
   xim= 0.25*(1.0-xi)
   xip= 0.25*(1.0+xi)
   x2p1=2.0*xi+1.0 
   x2m1=2.0*xi-1.0 
   e2p1=2.0*eta+1.0 
   e2m1=2.0*eta-1.0
   if nod == 3
     der[1,1] = 1.0
     der[1,3] = 0.0
     der[1,2] = -1.0
     der[2,1] = 0.0
     der[2,3] = 1.0
     der[2,2] = -1.0
   elseif nod == 6 
     der[1,1] = 4.0*c1-1.0 
     der[1,6] = 4.0*c2
     der[1,5] = 0.0  
     der[1,4] = -4.0*c2
     der[1,3] = -(4.0*c3-1.0)
     der[1,2] = 4.0*(c3-c1)
     der[2,1] = 0.0
     der[2,6] = 4.0*c1 
     der[2,5] = 4.0*c2-1.0
     der[2,4] = 4.0*(c3-c2)
     der[2,3] = -(4.0*c3-1.0)  
     der[2,2] = -4.0*c1
   elseif nod == 10
     der[1,1] = (27.0*c1*c1-18.0*c1+2.0)/2.0
     der[1,9] = (9.0*(6.0*c1-1.0)*c2)/2.0
     der[1,8] = (9.0*(3.0*c2-1.0)*c2)/2.0
     der[1,7] = 0.0
     der[1,6] = -(9.0*(3.0*c2-1.0)*c2)/2.0
     der[1,5] =  (9.0*(6.0*c1+6.0*c2-5.0)*c2)/2.0
     der[1,4] = -(27.0*c1*c1+54.0*c1*c2-36.0*c1+27.0*c2*c2-36.0*c2+11.0)/2.0
     der[1,3] =  (9.0*(9.0*c1*c1+12.0*c1*c2-10.0*c1+3.0*c2*c2-5.0*c2+2.0))/2.0
     der[1,2] = -(9.0*(9.0*c1*c1+6.0*c1*c2-8.0*c1-c2+1.0))/2.0
     der[1,10] = -27.0*(((c2-1.0)+c1)+c1)*c2
     der[2,1] = 0.0
     der[2,9] =  (9.0*(3.0*c1-1.0)*c1)/2.0
     der[2,8] =  (9.0*(6.0*c2-1.0)*c1)/2.0
     der[2,7] = (27.0*c2*c2-18.0*c2+2.0)/2.0
     der[2,6] = -(9.0*((c1+c2-1.0)*(6.0*c2-1.0)+(3.0*c2-1.0)*c2))/2.0
     der[2,5] =  (9.0*(3.0*c1*c1+12.0*c1*c2-5.0*c1+9.0*c2*c2-10.0*c2+2.0))/2.0
     der[2,4] = -(27.0*c1*c1+54.0*c1*c2-36.0*c1+27.0*c2*c2-36.0*c2+11.0)/2.0
     der[2,3] =  (9.0*(6.0*c1+6.0*c2-5.0)*c1)/2.0
     der[2,2] = -(9.0*(3.0*c1-1.0)*c1)/2.0
     der[2,10] = -27.0*(((c2-1.0)+c1)+c2)*c1
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
     der[1,1] = 32.0/3.0*(t2*t3*(t1+c1)+c1*t1*(t3+t2))
     der[1,12] = 128.0/3.0*c2*(t2*(t1+c1)+c1*t1) 
     der[1,11] = 64.0*c2*t4*(t1+c1)
     der[1,10] = 128.0/3.0*c2*t4*t5  
     der[1,9] = 0.0 
     der[1,8] = -128.0/3.0*c2*t4*t5
     der[1,7] = -64.0*c2*t4*(t7+c3) 
     der[1,6] = -128.0/3.0*c2*(t8*(t7+c3)+c3*t7)
     der[1,5] = -32.0/3.0*(t8*t9*(t7+c3)+c3*t7*(t8+t9))
     der[1,4] = 128.0/3.0*(c3*t7*t8-c1*(t8*(t7+c3)+c3*t7))
     der[1,3] = 64.0*(c3*t7*(t1+c1)-c1*t1*(t7+c3))
     der[1,2] = 128.0/3.0*(c3*(t2*(t1+c1)+c1*t1)-c1*t1*t2)
     der[1,13] = 128.0*c2*(c3*(t1+c1)-c1*t1) 
     der[1,15] = 128.0*c2*t4*(c3-c1)
     der[1,14] = 128.0*c2*(c3*t7-c1*(t7+c3))
     der[2,1] = 0.0 
     der[2,12] = 128.0/3.0*c1*t1*t2
     der[2,11] = 64.0*c1*t1*(t4+c2)
     der[2,10] = 128.0/3.0*c1*(t5*(t4+c2)+c2*t4)
     der[2,9] = 32.0/3.0*(t5*t6*(t4+c2)+c2*t4*(t6+t5))
     der[2,8] = 128.0/3.0*((c3*(t5*(t4+c2)+c2*t4))-c2*t4*t5)
     der[2,7] = 64.0*(c3*t7*(t4+c2)-c2*t4*(t7+c3))
     der[2,6] = 128.0/3.0*(c3*t7*t8-c2*(t8*(t7+c3)+c3*t7))
     der[2,5] = -32.0/3.0*(t8*t9*(t7+c3)+c3*t7*(t8+t9))
     der[2,4] = -128.0/3.0*c1*(t8*(t7+c3)+c3*t7)
     der[2,3] = -64.0*c1*t1*(t7+c3)  
     der[2,2] = -128.0/3.0*c1*t1*t2
     der[2,13] = 128.0*c1*t1*(c3-c2)
     der[2,15] = 128.0*c1*(c3*(t4+c2)-c2*t4)
     der[2,14] = 128.0*c1*(c3*t7-c2*(c3+t7))
   elseif nod == 4
     der[1,1] = -etam
     der[1,2] = -etap
     der[1,3] = etap
     der[1,4] = etam
     der[2,1] = -xim
     der[2,2] = xim
     der[2,3] = xip
     der[2,4] = -xip
   elseif nod == 5
     der[1,1] = -etam+0.5*xi*(1.0-eta*eta)
     der[1,2] = -etap+0.5*xi*(1.0-eta*eta)
     der[1,3] = etap+0.5*xi*(1.0-eta*eta)
     der[1,4] = etam+0.5*xi*(1.0-eta*eta)
     der[1,5] = -2.0*xi*(1.0-eta*eta)
     der[2,1] = -xim+0.5*eta*(1.0-xi*xi)
     der[2,2] = xim+0.5*eta*(1.0-xi*xi)
     der[2,3] = xip+0.5*eta*(1.0-xi*xi)
     der[2,4] = -xip+0.5*eta*(1.0-xi*xi)
     der[2,5] = -2.0*eta*(1.0-xi*xi)
   elseif nod == 8
     der[1,1] = etam*(2.0*xi+eta)
     der[1,2] = -8.0*etam*etap
     der[1,3] = etap*(2.0*xi-eta)
     der[1,4] = -4.0*etap*xi
     der[1,5] = etap*(2.0*xi+eta)
     der[1,6] = 8.0*etap*etam
     der[1,7] = etam*(2.0*xi-eta)
     der[1,8] = -4.0*etam*xi
     der[2,1] = xim*(xi+2.0*eta)
     der[2,2] = -4.0*xim*eta
     der[2,3] = xim*(2.0*eta-xi)
     der[2,4] = 8.0*xim*xip
     der[2,5] = xip*(xi+2.0*eta)
     der[2,6] = -4.0*xip*eta
     der[2,7] = xip*(2.0*eta-xi)
     der[2,8] = -8.0*xim*xip   
   elseif nod == 9
     etam=eta-1.0
     etap=eta+1.0
     xim=xi-1.0
     xip=xi+1.0
     der[1,1] = 0.25*x2m1*eta*etam  
     der[1,2] = -0.5*x2m1*etap*etam
     der[1,3] = 0.25*x2m1*eta*etap  
     der[1,4] = -xi*eta*etap
     der[1,5] = 0.25*x2p1*eta*etap  
     der[1,6] = -0.5*x2p1*etap*etam
     der[1,7] = 0.25*x2p1*eta*etam  
     der[1,8] = -xi*eta*etam
     der[1,9] = 2.0*xi*etap*etam    
     der[2,1] = 0.25*xi*xim*e2m1
     der[2,2] = -xi*xim*eta        
     der[2,3] = 0.25*xi*xim*e2p1
     der[2,4] = -0.5*xip*xim*e2p1   
     der[2,5] = 0.25*xi*xip*e2p1
     der[2,6] = -xi*xip*eta        
     der[2,7] = 0.25*xi*xip*e2m1
     der[2,8] = -0.5*xip*xim*e2m1   
     der[2,9] = 2.0*xip*xim*eta
   else
     println("Wrong number of nodes in shape_der.")
   end
  elseif ndim == 3                                          # 3.0 dimensional fin_els
   xi=points[i, 1]
   eta=points[i, 2]
   zeta=points[i, 3]
   etam=1.0-eta 
   xim=1.0-xi
   zetam=1.0-zeta
   etap=eta+1.0 
   xip=xi+1.0 
   zetap=zeta+1.0
   if nod == 4
     der[1:3,1:4] .= 0.0
     der[1,1] = 1.0
     der[2,2] = 1.0  
     der[3,3] = 1.0
     der[1,4] = -1.0 
     der[2,4] = -1.0 
     der[3,4] = -1.0  
   elseif nod == 8
     der[1,1] = -0.125*etam*zetam    
     der[1,2] = -0.125*etam*zetap
     der[1,3] =  0.125*etam*zetap     
     der[1,4] =  0.125*etam*zetam
     der[1,5] = -0.125*etap*zetam    
     der[1,6] = -0.125*etap*zetap
     der[1,7] =  0.125*etap*zetap     
     der[1,8] =  0.125*etap*zetam
     der[2,1] = -0.125*xim*zetam     
     der[2,2] = -0.125*xim*zetap
     der[2,3] = -0.125*xip*zetap     
     der[2,4] = -0.125*xip*zetam
     der[2,5] =  0.125*xim*zetam      
     der[2,6] =  0.125*xim*zetap
     der[2,7] =  0.125*xip*zetap      
     der[2,8] =  0.125*xip*zetam
     der[3,1] = -0.125*xim*etam      
     der[3,2] =  0.125*xim*etam
     der[3,3] =  0.125*xip*etam       
     der[3,4] = -0.125*xip*etam
     der[3,5] = -0.125*xim*etap      
     der[3,6] =  0.125*xim*etap
     der[3,7] =  0.125*xip*etap       
     der[3,8] = -0.125*xip*etap  
   elseif nod == 14                                     # type 6 fin_el
     der[1,1] =  (2.0*xi*eta+2.0*xi*zeta+4.0*xi+eta*zeta+eta+zeta)*
       (eta-1.0)*(zeta-1.0)/8.0
     der[1,2] = -(2.0*xi*eta-2.0*xi*zeta+4.0*xi-eta*zeta+eta-zeta)*
       (eta-1.0)*(zeta+1.0)/8.0
     der[1,3] = -(2.0*xi*eta-2.0*xi*zeta+4.0*xi+eta*zeta-eta+zeta)*
       (eta-1.0)*(zeta+1.0)/8.0
     der[1,4] =  (2.0*xi*eta+2.0*xi*zeta+4.0*xi-eta*zeta-eta-zeta)*
       (eta-1.0)*(zeta-1.0)/8.0
     der[1,5] =  -(eta-1.0)*(zeta+1.0)*(zeta-1.0)*xi 
     der[1,6] = -(eta+1.0)*(eta-1.0)*(zeta+1.0)*(zeta-1.0)/2.0
     der[1,7] =   (eta+1.0)*(eta-1.0)*(zeta+1.0)*xi
     der[1,8] =  (eta+1.0)*(eta-1.0)*(zeta+1.0)*(zeta-1.0)/2.0
     der[1,9] =  -(eta+1.0)*(eta-1.0)*(zeta-1.0)*xi  
     der[1,10] =  (2.0*xi*eta-2.0*xi*zeta-4.0*xi+eta*zeta+eta-zeta)*
       (eta+1.0)*(zeta-1.0)/8.0
     der[1,11] = -(2.0*xi*eta+2.0*xi*zeta-4.0*xi-eta*zeta+eta+zeta)*
       (eta+1.0)*(zeta+1.0)/8.0
     der[1,12] = -(2.0*xi*eta+2.0*xi*zeta-4.0*xi+eta*zeta-eta-zeta)*
       (eta+1.0)*(zeta+1.0)/8.0
     der[1,13] =  (2.0*xi*eta-2.0*xi*zeta-4.0*xi-eta*zeta-eta+zeta)*
       (eta+1.0)*(zeta-1.0)/8.0
     der[1,14] =   (eta+1.0)*(zeta+1.0)*(zeta-1.0)*xi
     der[2,1] =  (2.0*xi*eta+xi*zeta+xi+2.0*eta*zeta+4.0*eta+zeta)*
       (xi-1.0)*(zeta-1.0)/8.0                                  
     der[2,2] = -(2.0*xi*eta-xi*zeta+xi-2.0*eta*zeta+4.0*eta-zeta)*
       (xi-1.0)*(zeta+1.0)/8.0
     der[2,3] = -(2.0*xi*eta-xi*zeta+xi+2.0*eta*zeta-4.0*eta+zeta)*
       (xi+1.0)*(zeta+1.0)/8.0
     der[2,4] =  (2.0*xi*eta+xi*zeta+xi-2.0*eta*zeta-4.0*eta-zeta)*
       (xi+1.0)*(zeta-1.0)/8.0
     der[2,5] = -(xi+1.0)*(xi-1.0)*(zeta+1.0)*(zeta-1.0)/2.0
     der[2,6] =  -(xi-1.0)*(zeta+1.0)*(zeta-1.0)*eta
     der[2,7] =   (xi+1.0)*(xi-1.0)*(zeta+1.0)*eta
     der[2,8] =   (xi+1.0)*(zeta+1.0)*(zeta-1.0)*eta
     der[2,9] =  -(xi+1.0)*(xi-1.0)*(zeta-1.0)*eta
     der[2,10] =  (2.0*xi*eta-xi*zeta-xi+2.0*eta*zeta+4.0*eta-zeta)*
       (xi-1.0)*(zeta-1.0)/8.0
     der[2,11] = -(2.0*xi*eta+xi*zeta-xi-2.0*eta*zeta+4.0*eta+zeta)*
       (xi-1.0)*(zeta+1.0)/8.0
     der[2,12] = -(2.0*xi*eta+xi*zeta-xi+2.0*eta*zeta-4.0*eta-zeta)*
       (xi+1.0)*(zeta+1.0)/8.0
     der[2,13] =  (2.0*xi*eta-xi*zeta-xi-2.0*eta*zeta-4.0*eta+zeta)*
       (xi+1.0)*(zeta-1.0)/8.0
     der[2,14] =  (xi+1.0)*(xi-1.0)*(zeta+1.0)*(zeta-1.0)/2.0
     der[3,1] =  (xi*eta+2.0*xi*zeta+xi+2.0*eta*zeta+eta+4.0*zeta)*
       (xi-1.0)*(eta-1.0)/8.0
     der[3,2] = -(xi*eta-2.0*xi*zeta+xi-2.0*eta*zeta+eta-4.0*zeta)*
       (xi-1.0)*(eta-1.0)/8.0
     der[3,3] = -(xi*eta-2.0*xi*zeta+xi+2.0*eta*zeta-eta+4.0*zeta)*
       (xi+1.0)*(eta-1.0)/8.0
     der[3,4] =  (xi*eta+2.0*xi*zeta+xi-2.0*eta*zeta-eta-4.0*zeta)*
       (xi+1.0)*(eta-1.0)/8.0
     der[3,5] =  -(xi+1.0)*(xi-1.0)*(eta-1.0)*zeta  
     der[3,6] =  -(xi-1.0)*(eta+1.0)*(eta-1.0)*zeta  
     der[3,7] =  (xi+1.0)*(xi-1.0)*(eta+1.0)*(eta-1.0)/2.0
     der[3,8] =   (xi+1.0)*(eta+1.0)*(eta-1.0)*zeta
     der[3,9] = -(xi+1.0)*(xi-1.0)*(eta+1.0)*(eta-1.0)/2.0
     der[3,10] =  (xi*eta-2.0*xi*zeta-xi+2.0*eta*zeta+eta-4.0*zeta)*
       (xi-1.0)*(eta+1.0)/8.0
     der[3,11] = -(xi*eta+2.0*xi*zeta-xi-2.0*eta*zeta+eta+4.0*zeta)*
       (xi-1.0)*(eta+1.0)/8.0
     der[3,12] = -(xi*eta+2.0*xi*zeta-xi+2.0*eta*zeta-eta-4.0*zeta)*
       (xi+1.0)*(eta+1.0)/8.0
     der[3,13] =  (xi*eta-2.0*xi*zeta-xi-2.0*eta*zeta-eta+4.0*zeta)*
       (xi+1.0)*(eta+1.0)/8.0
     der[3,14] =   (xi+1.0)*(xi-1.0)*(eta+1.0)*zeta
   elseif nod == 20
     xii = [-1,-1,-1,0,1,1,1,0,-1,-1,1,1,-1,-1,-1,0,1,1,1,0]
     etai = [-1,-1,-1,-1,-1,-1,-1,-1,0,0,0,0,1,1,1,1,1,1,1,1]
     zetai = [-1,0,1,1,1,0,-1,-1,-1,1,1,-1,-1,0,1,1,1,0,-1,-1]
     for l in 1:20
       xi0=xi*xii[l]
       eta0=eta*etai[l]
       zeta0=zeta*zetai[l]
       if l == 4 || l == 8 || l == 16 || l == 20
         der[1,l] = -0.5*xi*(1.0+eta0)*(1.0+zeta0)
         der[2,l] = 0.25*etai[l]*(1.0-xi*xi)*(1.0+zeta0)
         der[3,l] = 0.25*zetai[l]*(1.0-xi*xi)*(1.0+eta0)
       elseif l >= 9 && l <= 12
         der[1,l] = 0.25*xii[l]*(1.0-eta*eta)*(1.0+zeta0)
         der[2,l] = -0.5*eta*(1.0+xi0)*(1.0+zeta0)
         der[3,l] = 0.25*zetai[l]*(1.0+xi0)*(1.0-eta*eta)
       elseif l == 2 || l == 6 || l == 14 || l == 18
         der[1,l] = 0.25*xii[l]*(1.0+eta0)*(1.0-zeta*zeta)
         der[2,l] = 0.25*etai[l]*(1.0+xi0)*(1.0-zeta*zeta)
         der[3,l] = -0.5*zeta*(1.0+xi0)*(1.0+eta0)
       else
         der[1,l] = 0.125*xii[l]*(1.0+eta0)*(1.0+zeta0)*
           (2.0*xi0+eta0+zeta0-1.0)
         der[2,l] = 0.125*etai[l]*(1.0+xi0)*(1.0+zeta0)*
           (xi0+2.0*eta0+zeta0-1.0)
         der[3,l] = 0.125*zetai[l]*(1.0+xi0)*(1.0+eta0)*
           (xi0+eta0+2.0*zeta0-1.0)
       end
     end 
   else
     println("Wrong number of nodes in shape_der.")
   end
  else
   println("Wrong number of dimensions in shape_der.")
  end
end