function rod_km!(km::Matrix{Float64}, ea::Float64, length::Float64)
  #
  # This subroutine forms the stiffness matrix of a 1-d "rod" element.
  #
 km = [1.0 -1.0;-1.0 1.0]
 km *= ea/length
end