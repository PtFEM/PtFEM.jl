#=
SUBROUTINE spabac(kv,loads,kdiag)
!
! This subroutine performs Cholesky forward and back-substitution
! on a symmetric skyline global matrix.
!
 IMPLICIT NONE
 INTEGER,PARAMETER::iwp=SELECTED_REAL_KIND(15)
 REAL(iwp),INTENT(IN)::kv(:)
 REAL(iwp),INTENT(IN OUT)::loads(0:)
 INTEGER,INTENT(IN)::kdiag(:)
 INTEGER::n,i,ki,l,m,j,it,k
 REAL(iwp)::x
 n=UBOUND(kdiag,1)
 loads(1)=loads(1)/kv(1)
 DO i=2,n
   ki=kdiag(i)-i
   l=kdiag(i-1)-ki+1 
   x=loads(i)
   IF(l/=i)THEN
     m=i-1
     DO j=l,m 
       x=x-kv(ki+j)*loads(j)
     END DO
   END IF
   loads(i)=x/kv(ki+i)
 END DO
 DO it=2,n
   i=n+2-it
   ki=kdiag(i)-i
   x=loads(i)/kv(ki+i)
   loads(i)=x
   l=kdiag(i-1)-ki+1
   IF(l/=i)THEN
     m=i-1
     DO k=l,m
       loads(k)=loads(k)-x*kv(ki+k)
     END DO
   END IF
 END DO
 loads(1)=loads(1)/kv(1)
RETURN
END SUBROUTINE spabac               
=#
#=
@doc doc"""
  This subroutine performs Cholesky factorisation on a symmetric
  skyline global matrix.

  spabac(kv, loads, kdiag)

  where:
    kv::Vector{Float64}   : Skyline vector of global stiffness matrix
    loads::Vector{Float64}: Load vector
    kdiag::Vector{Int64}  : Diagonal element vector
  """ ->
=#
function spabac!(kv::Vector{Float64}, loads::Vector{Float64}, kdiag::Vector{Int64})
  local x::Float64
  n = size(kdiag, 1)
  loads[1]=loads[1]/kv[1]
  for i in 2:n
    ki = kdiag[i]-i
    l = kdiag[i-1] - ki + 1
    x = loads[i]
    if l !== i
      m = i - 1
      for j in l:m
        x = x - kv[ki+j]*loads[j]
      end
    end
    loads[i] = x/kv[ki+i]
  end
  for it in 2:n
    i = n + 2 - it
    ki = kdiag[i] - i
    x = loads[i]/kv[ki+i]
    loads[i] = x
    l = kdiag[i-1] - ki + 1
    if l !== i
      m = i - 1
      for k in l:m
        loads[k] -= x*kv[ki+k]
      end
    end
  end
  loads[1]=loads[1]/kv[1]
end

