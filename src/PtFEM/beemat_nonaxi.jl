function beemat_nonaxi!(bee::Matrix{Float64}, radius::Float64, coord::Matrix{Float64}, deriv::Matrix{Float64}, fun::Vector{Float64}, iflag::Int64, lth::Int64)
  #
  # This subroutine forms the strain-displacement matrix for
  # axisymmetric solids subjected to non-axisymmetric loading.
  #
  # Updates bee, radius
  #
 nod = size(deriv, 2) 
 radius = sum(fun.*coord[:,1])
 for m in 1:nod
   n = 3 * m      
   k = n-1      
   l = k-1
   bee[1,l] = deriv[1, m]
   bee[2,k] = deriv[2, m]
   bee[3,l] = fun[m]/radius
   bee[3,n] = iflag*lth*bee[3, l] 
   bee[4,l] = deriv[2, m]
   bee[4,k] = deriv[1, m]
   bee[5,k] = -iflag*lth*fun[m]/radius 
   bee[5,n] = deriv[2, m]
   bee[6,l] = bee[5,k] 
   bee[6,n] = deriv[1, m]-fun[m]/radius
 end
 (radius, bee)
end