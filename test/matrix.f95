! matrix test file

subroutine vec(n, vector)

integer*8 n
real*8 vector(*)

do i=1,n
 vector(i) = 5.0 * vector(i)
end do

end

subroutine tten(m,n,o,matrix)

integer*8 m,n,o
real*8 matrix(m,n,o)

matrix(1,1,1) = 8.0 * matrix(1,1,1)
matrix = 10.0 * matrix

end

