"""
## rigid_jointed!

This subroutine forms the global stiffness matrix of a
general pin-joionted structural element (1-, 2- or 3-d).

### Function
```julia
rigid_jointed!(km, prop, gamma, etype, iel, coord)
```

### Arguments
```julia
* km::Matrix{Float64}       : Element stiffness matrix (Updated)
* prop::Matrix{Float64}     : Element properties
* gamma::Vector{Float64}    : Element orientations (3D)
* etype::Vector{Int}        : Element type vector
* iel::Int                  : Element number
* coord::Matrix{Float64}}   : Element nodal coordinates
```
"""
function rigid_jointed!(km::Matrix{Float64}, prop::Matrix{Float64}, 
  gamma::Vector{Float64}, etype::Vector{Int}, iel::Int, coord::LinearAlgebra.Adjoint{Float64,Array{Float64,2}}) 
#
# This function forms the stiffness matrix of a
# general beam/column fin_el (1-, 2- or 3-d).
#
 ndim=size(coord,2)
 @assert 1 <= ndim <= 3
 ndof=size(km,1)
 r0=zeros(3,3)
 t1=zeros(12,12)
 t2=zeros(12,12)
 cc=zeros(12,12)
 
 if ndim ==1
   ei=prop[etype[iel],1]
   ell=coord[2,1]-coord[1,1]
   km[1,1]=12.0*ei/(ell*ell*ell) 
   km[3,3]=km[1,1]
   km[1,2]=6.0*ei/(ell*ell) 
   km[2,1]=km[1,2]
   km[1,4]=km[1,2]
   km[4,1]=km[1,4] 
   km[1,3]=-km[1,1] 
   km[3,1]=km[1,3] 
   km[3,4]=-km[1,2]
   km[4,3]=km[3,4]
   km[2,3]=km[3,4] 
   km[3,2]=km[2,3]
   km[2,2]=4.0*ei/ell
   km[4,4]=km[2,2] 
   km[2,4]=2.0*ei/ell 
   km[4,2]=km[2,4]
 elseif ndim == 2
   ea=prop[etype[iel],1]
   ei=prop[etype[iel], 2]
   x1=coord[1,1]
   y1=coord[1,2]
   x2=coord[2,1]
   y2=coord[2,2]
   ell=sqrt((y2-y1)^2+(x2-x1)^2)
   c=(x2-x1)/ell
   s=(y2-y1)/ell
   e1=ea/ell
   e2=12.0*ei/(ell*ell*ell)
   e3=ei/ell
   e4=6.0*ei/(ell*ell)
   km[1,1]=c*c*e1+s*s*e2
   km[4,4]=km[1,1]
   km[1,2]=s*c*(e1-e2)
   km[2,1]=km[1,2]
   km[4,5]=km[1,2]
   km[5,4]=km[4,5]
   km[1,3]=-s*e4
   km[3,1]=km[1,3]
   km[1,6]=km[1,3]
   km[6,1]=km[1,6]
   km[3,4]=s*e4 
   km[4,3]=km[3,4]
   km[4,6]=km[3,4]
   km[6,4]=km[4,6]
   km[1,4]=-km[1,1] 
   km[4,1]=km[1,4]
   km[1,5]=s*c*(-e1+e2)
   km[5,1]=km[1,5]
   km[2,4]=km[1,5]
   km[4,2]=km[2,4]
   km[2,2]=s*s*e1+c*c*e2
   km[5,5]=km[2,2]
   km[2,5]=-km[2,2]
   km[5,2]=km[2,5]
   km[2,3]=c*e4
   km[3,2]=km[2,3]
   km[2,6]=km[2,3]
   km[6,2]=km[2,6]
   km[3,3]=4.0*e3
   km[6,6]=km[3,3]
   km[3,5]=-c*e4
   km[5,3]=km[3,5]
   km[5,6]=km[3,5]
   km[6,5]=km[5,6]
   km[3,6]=2.0*e3
   km[6,3]=km[3,6]
 else
   ea=prop[etype[iel],1]
   eiy=prop[etype[iel],2]
   eiz=prop[etype[iel],3]
   gj=prop[etype[iel],4]
   x1=coord[1,1]
   y1=coord[1,2]
   z1=coord[1,3]
   x2=coord[2,1]
   y2=coord[2,2]
   z2=coord[2,3]
   xl=x2-x1
   yl=y2-y1
   zl=z2-z1
   ell=sqrt(xl*xl+yl*yl+zl*zl)
   km=zeros(ndof,ndof)
   a1=ea/ell
   a2=12.0*eiz/(ell*ell*ell)
   a3=12.0*eiy/(ell*ell*ell)
   a4=6.0*eiz/(ell*ell)
   a5=6.0*eiy/(ell*ell)
   a6=4.0*eiz/ell
   a7=4.0*eiy/ell
   a8=gj/ell
   km[1,1]=a1
   km[7,7]=a1
   km[1,7]=-a1
   km[7,1]=-a1
   km[2,2]=a2
   km[8,8]=a2
   km[2,8]=-a2
   km[8,2]=-a2
   km[3,3]=a3
   km[9,9]=a3
   km[3,9]=-a3
   km[9,3]=-a3
   km[4,4]=a8
   km[10,10]=a8
   km[4,10]=-a8
   km[10,4]=-a8
   km[5,5]=a7
   km[11,11]=a7
   km[5,11]=0.5*a7
   km[11,5]=0.5*a7
   km[6,6]=a6
   km[12,12]=a6
   km[6,12]=0.5*a6
   km[12,6]=0.5*a6
   km[2,6]=a4
   km[6,2]=a4
   km[2,12]=a4
   km[12,2]=a4
   km[6,8]=-a4
   km[8,6]=-a4
   km[8,12]=-a4
   km[12,8]=-a4
   km[5,9]=a5
   km[9,5]=a5
   km[9,11]=a5
   km[11,9]=a5
   km[3,5]=-a5
   km[5,3]=-a5
   km[3,11]=-a5
   km[11,3]=-a5
   gamrad=gamma[iel]*pi/180.0
   cg=cos(gamrad)
   sg=sin(gamrad)
   den=ell*sqrt(xl*xl+zl*zl)
   if den !== 0.0
     r0[1,1]=xl/ell
     r0[1,2]=yl/ell
     r0[1,3]=zl/ell
     r0[2,1]=(-xl*yl*cg-ell*zl*sg)/den
     r0[2,2]=den*cg/(ell*ell)
     r0[2,3]=(-yl*zl*cg+ell*xl*sg)/den
     r0[3,1]=(xl*yl*sg-ell*zl*cg)/den
     r0[3,2]=-den*sg/(ell*ell)
     r0[3,3]=(yl*zl*sg+ell*xl*cg)/den
   else
     r0[1,1]=0.0
     r0[1,3]=0.0
     r0[2,2]=0.0
     r0[3,2]=0.0
     r0[1,2]=1.0
     r0[2,1]=-cg
     r0[3,3]=cg
     r0[2,3]=sg
     r0[3,1]=sg
   end
   for i in 1:3
     for j in 1:3 
       x=r0[i,j]
       for k in 0:3:9
         t1[i+k,j+k]=x
         t2[j+k,i+k]=x
       end
     end
   end
   for i in 1:12
     for j in 1:12
       tsum=0.0
       for k in 1:12
         tsum += km[i,k]*t1[k,j]
       end
       cc[i,j]=tsum
     end
   end
   for i in 1:12
     for j in 1:12
       tsum=0.0
       for k in 1:12
         tsum += t2[i,k]*cc[k,j]
       end
       km[i,j]=tsum
     end
   end
 end        # if ndim statement
 km
end