function rod_mm!(mm::Matrix{Float64}, length::Float64)
  #
  # This subroutine forms the consistent mass matrix of a 1-d "rod" fin_el.
  #
 mm = [1.0/3.0 1.0/6.0; 1.0/6.0 1.0/3.0]
 mm *= length
end