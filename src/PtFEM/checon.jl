function checon!(loads::Vector{Float64}, oldlds::Vector{Float64}, tol::Float64)
  #
  # This subroutine sets converged to .FALSE. if relative change in loads
  # and oldlds is greater than tol and updates oldlds.
  #
  # Updates oldlds
 converged = maximum(abs.(loads-oldlds))/maximum(abs.(loads)) <= tol
 oldlds[:] = loads
 converged
end