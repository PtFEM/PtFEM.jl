subroutine formnf(nodof, nn, nf)
!
! This subroutine forms the nf matrix.
!
  integer*8 nodof, nn
  integer*8 nf(nodof, nn)
  integer*8 i, j, m
  
  m = 0
  do j= 1, nn
   do i= 1, nodof
     if (nf(i, j) /= 0) then
       m = m + 1
       nf(i,j) = m
     end if
   end do
  end do
return
end subroutine formnf 

subroutine num_to_g(nod, nodof, nn, ndof, num, nf, g)
!
! This subroutine finds the g vector from num and nf.
!
  integer*8 nod, nodof, nn, ndof
  integer*8 num(nod), nf(nodof,nn)  
  integer*8 g(ndof)
  integer*8 i, k

  do i = 1, nod
   k = i * nodof
   g(k-nodof+1:k) = nf(:, num(i))
  end do
  return
end subroutine num_to_g   

subroutine fkdiag(ndof, neq, g, kdiag)
!
! This subroutine computes the skyline profile.
!
 integer*8 ndof, neq
 integer*8 g(ndof)
 integer*8 kdiag(neq)
 integer*8 i, iwp1, j, im, k
 
 do i=1, ndof
   iwp1 = 1
   if (g(i) /= 0) then
     do j=1, ndof
       if (g(j) /= 0) then
         im = g(i)-g(j) + 1
         if (im > iwp1) iwp1 = im
       end if
     end do
     k=g(i)
     if (iwp1 > kdiag(k)) kdiag(k) = iwp1
   end if
 end do
return
end subroutine fkdiag

subroutine rigid_jointed(ndof, nprops, np_types, nels, nod, ndim, km, prop, gamma, etype, iel, coord) 
!
! This subroutine forms the stiffness matrix of a
! general beam/column element (1-, 2- or 3-d).
!
  integer*8 ndof, nprops, np_types, nels, ndim
  real*8 prop(nprops,np_types), gamma(nels), coord(nod, ndim)
  integer*8 etype(nels), iel
  real*8 km(ndof, ndof)
  integer*8 i,j,k
  real*8 ell,x1,x2,y1,y2,z1,z2,c,s,e1,e2,e3,e4,pi,xl,yl,zl,cg,sg,den,   &
   ea,ei,eiy,eiz,gj,a1,a2,a3,a4,a5,a6,a7,a8,sum,gamrad,x,t(12,12),      &
   tt(12,12),cc(12,12),r0(3,3),zero,pt5,one,two,d4,d6,d12,d180
   
  zero = 0.0
  pt5 = 0.5
  one = 1.0
  two = 2.0
  d4 = 4.0
  d6 = 6.0
  d12 = 12.0
  d180 = 180.0
  
  select case(ndim)
  case(1)
   ei=prop(1,etype(iel))
   ell=coord(2,1)-coord(1,1)
   km(1,1)=d12*ei/(ell*ell*ell) 
   km(3,3)=km(1,1)
   km(1,2)=d6*ei/(ell*ell) 
   km(2,1)=km(1,2) 
   km(1,4)=km(1,2)
   km(4,1)=km(1,4) 
   km(1,3)=-km(1,1) 
   km(3,1)=km(1,3) 
   km(3,4)=-km(1,2)
   km(4,3)=km(3,4) 
   km(2,3)=km(3,4) 
   km(3,2)=km(2,3)
   km(2,2)=d4*ei/ell
   km(4,4)=km(2,2) 
   km(2,4)=two*ei/ell 
   km(4,2)=km(2,4)
  case(2)
   ea=prop(1,etype(iel))
   ei=prop(2,etype(iel))
   x1=coord(1,1)
   y1=coord(1,2)
   x2=coord(2,1)
   y2=coord(2,2)
   ell=SQRT((y2-y1)**2+(x2-x1)**2)
   c=(x2-x1)/ell
   s=(y2-y1)/ell
   e1=ea/ell
   e2=d12*ei/(ell*ell*ell)
   e3=ei/ell
   e4=d6*ei/(ell*ell)
   km(1,1)=c*c*e1+s*s*e2
   km(4,4)=km(1,1)
   km(1,2)=s*c*(e1-e2)
   km(2,1)=km(1,2)
   km(4,5)=km(1,2)
   km(5,4)=km(4,5)
   km(1,3)=-s*e4
   km(3,1)=km(1,3)
   km(1,6)=km(1,3)
   km(6,1)=km(1,6)
   km(3,4)=s*e4 
   km(4,3)=km(3,4)
   km(4,6)=km(3,4)
   km(6,4)=km(4,6)
   km(1,4)=-km(1,1) 
   km(4,1)=km(1,4)
   km(1,5)=s*c*(-e1+e2)
   km(5,1)=km(1,5)
   km(2,4)=km(1,5)
   km(4,2)=km(2,4)
   km(2,2)=s*s*e1+c*c*e2
   km(5,5)=km(2,2)
   km(2,5)=-km(2,2)
   km(5,2)=km(2,5)
   km(2,3)=c*e4
   km(3,2)=km(2,3)
   km(2,6)=km(2,3)
   km(6,2)=km(2,6)
   km(3,3)=d4*e3
   km(6,6)=km(3,3)
   km(3,5)=-c*e4
   km(5,3)=km(3,5)
   km(5,6)=km(3,5)
   km(6,5)=km(5,6)
   km(3,6)=two*e3
   km(6,3)=km(3,6)
  case(3)
   ea=prop(1,etype(iel))
   eiy=prop(2,etype(iel))
   eiz=prop(3,etype(iel))
   gj=prop(4,etype(iel))
   x1=coord(1,1)
   y1=coord(1,2)
   z1=coord(1,3)
   x2=coord(2,1)
   y2=coord(2,2)
   z2=coord(2,3)
   xl=x2-x1
   yl=y2-y1
   zl=z2-z1
   ell=sqrt(xl*xl+yl*yl+zl*zl)
   km=zero
   t=zero
   tt=zero
   a1=ea/ell
   a2=d12*eiz/(ell*ell*ell)
   a3=d12*eiy/(ell*ell*ell)
   a4=d6*eiz/(ell*ell)
   a5=d6*eiy/(ell*ell)
   a6=d4*eiz/ell
   a7=d4*eiy/ell
   a8=gj/ell
   km(1,1)=a1
   km(7,7)=a1
   km(1,7)=-a1
   km(7,1)=-a1
   km(2,2)=a2
   km(8,8)=a2
   km(2,8)=-a2
   km(8,2)=-a2
   km(3,3)=a3
   km(9,9)=a3
   km(3,9)=-a3
   km(9,3)=-a3
   km(4,4)=a8
   km(10,10)=a8
   km(4,10)=-a8
   km(10,4)=-a8
   km(5,5)=a7
   km(11,11)=a7
   km(5,11)=pt5*a7
   km(11,5)=pt5*a7
   km(6,6)=a6
   km(12,12)=a6
   km(6,12)=pt5*a6
   km(12,6)=pt5*a6
   km(2,6)=a4
   km(6,2)=a4
   km(2,12)=a4
   km(12,2)=a4
   km(6,8)=-a4
   km(8,6)=-a4
   km(8,12)=-a4
   km(12,8)=-a4
   km(5,9)=a5
   km(9,5)=a5
   km(9,11)=a5
   km(11,9)=a5
   km(3,5)=-a5
   km(5,3)=-a5
   km(3,11)=-a5
   km(11,3)=-a5
   pi=acos(-one)
   gamrad=gamma(iel)*pi/d180
   cg=cos(gamrad)
   sg=sin(gamrad)
   den=ell*sqrt(xl*xl+zl*zl)
   if (den /= zero) then
     r0(1,1)=xl/ell
     r0(1,2)=yl/ell
     r0(1,3)=zl/ell
     r0(2,1)=(-xl*yl*cg-ell*zl*sg)/den
     r0(2,2)=den*cg/(ell*ell)
     r0(2,3)=(-yl*zl*cg+ell*xl*sg)/den
     r0(3,1)=(xl*yl*sg-ell*zl*cg)/den
     r0(3,2)=-den*sg/(ell*ell)
     r0(3,3)=(yl*zl*sg+ell*xl*cg)/den
   else
     r0(1,1)=zero
     r0(1,3)=zero
     r0(2,2)=zero
     r0(3,2)=zero
     r0(1,2)=one
     r0(2,1)=-cg
     r0(3,3)=cg
     r0(2,3)=sg
     r0(3,1)=sg
   end if
     do i=1,3
       do j=1,3 
       x=r0(i,j)
       do k=0,9,3
         t(i+k,j+k)=x
         tt(j+k,i+k)=x
       end do
     end do
   end do
   do i=1,12
     do j=1,12
       sum=zero
       do k=1,12
         sum=sum+km(i,k)*t(k,j)
       end do
       cc(i,j)=sum
     end do
   end do
   do i=1,12
     do j=1,12
       sum=zero
       do k=1,12
         sum=sum+tt(i,k)*cc(k,j)
       end do
       km(i,j)=sum
     end do
   end do
  end select
return
end subroutine rigid_jointed                

subroutine fsparv(kdiag_neq, ndof, neq, kv, km, g, kdiag)
!
! This subroutine assembles element matrices into a symmetric skyline
! global matrix.
!
 integer*8 g(ndof), kdiag(neq)
 real*8 km(ndof, ndof)
 real*8 kv(kdiag_neq) 
 integer*8 i,idof,k,j,iw,ival
 do i=1,ndof
   k = g(i)
   if (k /= 0) then
     do j = 1,ndof
       if (g(j)/=0) then
         iw = k - g(j)
         if (iw >= 0) then
           ival = kdiag(k) - iw
           kv(ival) = kv(ival) + km(i,j) 
         end if
       end if
     end do
   end if
 end do
return
end subroutine fsparv

subroutine sparin(kdiag_neq, neq, kv, kdiag)
!
! This subroutine performs Cholesky factorisation on a symmetric
! skyline global matrix.
!
  integer*8 kdiag_neq, neq
  integer*8 kdiag(neq)
  real*8 kv(kdiag_neq) 
  integer*8 n, i, ki, l, kj, j, ll, m, k
  real*8 x
  n=neq  
  kv(1) = sqrt(kv(1))
  do i=2, n
   ki = kdiag(i) - i
   l = kdiag(i-1) - ki+1
   do j = l, i
     x = kv(ki+j)
     kj = kdiag(j)-j
     if (j /= 1) then
       ll = kdiag(j-1) - kj + 1
       ll = max(l, ll)
       if (ll /= j) then
         m = j - 1
         do k = ll, m 
           x = x - kv(ki+k) * kv(kj + k) 
         end do
       end if
     end if
     kv(ki+j) = x / kv(kj +  j)
   end do
   kv(ki+i) = sqrt(x)
  end do
return
end subroutine sparin

 subroutine  spabac(kdiag_neq, length_loads, neq, kv, loads, kdiag)
!
! This subroutine performs Cholesky forward and back-substitution
! on a symmetric skyline global matrix.
!
  integer*8 kdiag_neq, length_loadsneq
  integer*8 kdiag(neq)
  real*8 loads(length_loads), kv(kdiag_neq) 
  integer*8 n, i, ki, l, m, j, it, k
  real*8 x
  n = neq
  loads(1) = loads(1) / kv(1)
  do i = 2, n
   ki = kdiag(i) - i
   l = kdiag(i - 1) - ki + 1 
   x = loads(i)
   if (l /= i)  then 
     m = i - 1
     do j = l, m 
       x = x - kv(ki + j) * loads(j)
     end do
   end if
   loads(i) = x / kv(ki + i)
  end do
  do it = 2, n
   i = n + 2 - it
   ki = kdiag(i) - i
   x = loads(i) / kv(ki + i)
   loads(i) = x
   l = kdiag(i - 1) - ki + 1
   if (l /= i) then 
     m = i - 1
     do k =l, m
       loads(k) = loads(k) - x * kv(ki + k)
     end do
   end if
  end do
  loads(1) = loads(1) / kv(1)
return
end  subroutine  spabac
