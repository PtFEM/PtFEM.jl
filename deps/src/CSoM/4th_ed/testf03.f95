subroutine testf03(nodof, nn, nf)
!
! Test xxxx.f03 subroutine.
!
  integer*8 nodof, nn
  integer*8 nf(nodof, nn)
 
  nf(1,1) = UBOUND(nf,2)
  nf(2,1) = UBOUND(nf,1)
return
end subroutine testf03

