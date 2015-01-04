SUBROUTINE bc_rect(nxe,nye,nf,dir)
!
! This subroutine generates the nf array for a rectangular mesh
! of 8-node quadrilaterals fully fixed on the base and with
! vertical rollers on the left and right sides. Nodes numbered
! in the x- or y-direction
!
 IMPLICIT NONE
 INTEGER,INTENT(IN)::nxe,nye
 CHARACTER(LEN=1),INTENT(IN)::dir
 INTEGER,INTENT(OUT)::nf(:,:)
 INTEGER::nm,ic,i,j
!
 IF(dir=='y')THEN            
   nm=0
   ic=0
   DO j=1,2*nye
     nm=nm+1
     nf(1,nm)=0
     ic=ic+1
     nf(2,nm)=ic
   END DO
   nm=nm+1
   nf(1,nm)=0
   nf(2,nm)=0
  !
   DO i=1,nxe-1
     DO j=1,nye
       nm=nm+1
       ic=ic+1
       nf(1,nm)=ic
       ic=ic+1
       nf(2,nm)=ic  
     END DO
     nm=nm+1
     nf(1,nm)=0
     nf(2,nm)=0   
  !
     DO j=1,2*nye
       nm=nm+1
       ic=ic+1
       nf(1,nm)=ic
       ic=ic+1
       nf(2,nm)=ic
     END DO
     nm=nm+1
     nf(1,nm)=0
     nf(2,nm)=0
   END DO
  !
   DO j=1,nye
     nm=nm+1
     ic=ic+1
     nf(1,nm)=ic
     ic=ic+1
     nf(2,nm)=ic  
   END DO
   nm=nm+1
   nf(1,nm)=0
   nf(2,nm)=0   
  !
   DO j=1,2*nye
     nm=nm+1
     nf(1,nm)=0
     ic=ic+1
     nf(2,nm)=ic
   END DO
   nm=nm+1
   nf(1,nm)=0
   nf(2,nm)=0
 ELSE
   nm=0
   ic=0
   DO j=1,nye
     nm=nm+1
     nf(1,nm)=0
     ic=ic+1
     nf(2,nm)=ic
     DO i=1,2*nxe-1
       nm=nm+1
       ic=ic+1
       nf(1,nm)=ic
       ic=ic+1
       nf(2,nm)=ic  
     END DO
     nm=nm+1
     nf(1,nm)=0
     ic=ic+1
     nf(2,nm)=ic
!
     nm=nm+1
     nf(1,nm)=0
     ic=ic+1
     nf(2,nm)=ic
     DO i=1,nxe-1
       nm=nm+1
       ic=ic+1
       nf(1,nm)=ic
       ic=ic+1
       nf(2,nm)=ic  
     END DO
     nm=nm+1
     nf(1,nm)=0
     ic=ic+1
     nf(2,nm)=ic
   END DO
!
   DO i=1,2*nxe+1
     nm=nm+1
     nf(1,nm)=0
     nf(2,nm)=0
   END DO
 END IF
 RETURN
END SUBROUTINE bc_rect
