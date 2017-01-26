function checon!(loads::Vector{Float64}, oldlds::Vector{Float64}, tol::Float64,  converged::Bool)
  #
  # This subroutine sets converged to .FALSE. if relative change in loads
  # and oldlds is greater than tol and updates oldlds.
  #
  # Updates converged and oldlds
 converged = true
 #converged = (maximum(abs(loads-oldlds))/maximum(abs(loads)) <= tol)
 converged = (maximum(abs.(loads-oldlds))/maximum(abs.(loads)) <= tol)
 oldlds[:] = loads
 converged
end