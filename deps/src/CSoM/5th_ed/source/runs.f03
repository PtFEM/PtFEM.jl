PROGRAM run
 IMPLICIT NONE
 INTEGER::narg
 INTEGER::nlen
 INTEGER::iargc
 CHARACTER(LEN=15)::progname,dataname
 LOGICAL found
 narg=iargc()
 IF(narg.lt.1)THEN
   WRITE(*,*)'Please enter the base name of program file: '
   READ(*,*) progname
   WRITE(*,*)'Please enter the base name of data file: '
   READ(*,*) dataname
  ELSEIF (narg.lt.2)THEN
   CALL getarg(1,progname)
   WRITE(*,*)'Please enter the base name of data file: '
   READ(*,*) dataname
  ELSE
   CALL getarg(1,progname)
   CALL getarg(2,dataname)
 ENDIF
 nlen=lnblnk(progname)
 INQUIRE(file=progname(1:nlen)//'.f03',exist=found)
 IF(.not.found)THEN
  WRITE(*,*)'Program file not found: ',progname(1:nlen)//'.f03'
  WRITE(*,*)'Please create or check spelling.'
  STOP
 ENDIF
 nlen=lnblnk(dataname)
 INQUIRE(file=dataname(1:nlen)//'.dat',exist=found)
 IF(.not.found)THEN
  WRITE(*,*)'Data file not found: ',dataname(1:nlen)//'.dat'
  WRITE(*,*)'Please create or check spelling.'
  STOP
 ENDIF
 call system('run5.bat '//progname//' '//dataname)
STOP
END PROGRAM run
