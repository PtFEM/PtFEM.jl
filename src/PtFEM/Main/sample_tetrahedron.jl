function sample!(fin_el::Tetrahedron, s::Matrix{Float64} , wt::Vector{Float64})
  #
  # This subroutine returns the local coordinates and weighting coefficients
  # of the integrating points.
  #
  
  root3 = 1.0 / sqrt(3.0)
  r15 = 0.2 * sqrt(15.0)
  nip = size(s,1)

  w = [5.0/9.0, 8.0/9.0, 5.0/9.0]
  v = [5.0/9.0*w; 8.0/9.0*w; 5.0/9.0*w]
 
  if nip == 1
    s[1,1] = 0.25
    s[1,2] = 0.25
    s[1,3] = 0.25
    wt[1] = 1//6
  elseif nip == 4
    s[1,1] = 0.58541020
    s[1,2] = 0.13819660
    s[1,3] = s[1,2]
    s[2,2] = s[1,1]
    s[2,3] = s[1,2]
    s[2,1] = s[1,2]
    s[3,3] = s[1,1]
    s[3,1] = s[1,2]
    s[3,2] = s[1,2]
    s[4,1] = s[1,2]
    s[4,2] = s[1,2]
    s[4,3] = s[1,2]
    wt[1:4] = 0.25/6.0
  elseif nip == 5
    s[1,1:3] = 0.25
    s[2,1] = 0.5
    s[2,2] = 1 // 6
    s[2,3] = s[2,2]
    s[3,2] = 0.5
    s[3,3] = 1 // 6
    s[3,1] = s[3,3]
    s[4,3] = 0.5
    s[4,1] = 1 // 6
    s[4,2] = s[4,1]
    s[5,1] = 1 // 6
    s[5,2] = s[5,1]
    s[5,3] = s[5,1]
    wt[1] = -0.8/6.0
    wt[2] = 9 // 120
    wt[3:5] = wt[2]
    wt[:] = wt[:]/6.0
  else
    println("Wrong number of integrating points for a tetrahedron.")
  end
end
