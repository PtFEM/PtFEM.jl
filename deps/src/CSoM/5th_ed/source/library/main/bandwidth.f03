FUNCTION bandwidth(g) RESULT(nband)
!
! This function finds the element bandwidth from g.
!
 IMPLICIT NONE     
 INTEGER,INTENT(IN)::g(:) 
 INTEGER::nband
 nband=MAXVAL(g,1,g>0)-MINVAL(g,1,g>0)
RETURN
END FUNCTION bandwidth

