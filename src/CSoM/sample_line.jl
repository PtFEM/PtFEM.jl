function sample!(fin_el::Line, s::Matrix{Float64} , wt::Vector{Float64})
  #
  # This subroutine returns the local coordinates and weighting coefficients
  # of the integrating points.
  #
  
  nip = size(s,1)
  (s[:,1], wt[:]) = Base.gauss(Float64, nip)
end
