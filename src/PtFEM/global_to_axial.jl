function global_to_axial(glob::Vector{Float64}, coord::Matrix{Float64})
  #
  # This subroutine transforms the global end reactions
  # into an axial force for rod fin_els (2- or 3-d).
  #
  local axial = 0.0
  local ndim = size(coord,2)
  local add = 0.0
  for i in 1:ndim
    add = add+(coord[2,i]-coord[1,i])^2
  end
  ell = sqrt(add)
  for i in 1:ndim
    axial = axial+(coord[2,i]-coord[1,i])/ell*glob[ndim+i]
  end
  axial
end