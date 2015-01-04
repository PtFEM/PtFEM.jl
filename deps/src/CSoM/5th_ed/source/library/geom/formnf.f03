SUBROUTINE formnf(nf)
!
! This subroutine forms the nf matrix.
!
 IMPLICIT NONE
 INTEGER,INTENT(IN OUT)::nf(:,:)
 INTEGER::i,j,m
 m=0
 DO j=1,UBOUND(nf,2)
   DO i=1,UBOUND(nf,1)
     IF(nf(i,j)/=0)THEN
       m=m+1
       nf(i,j)=m
     END IF
   END DO
 END DO
RETURN
END SUBROUTINE formnf 
