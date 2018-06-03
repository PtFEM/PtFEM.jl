function sample!(fin_el::Hexahedron, s::Matrix{Float64} , wt::Vector{Float64})
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
    s[1,1:3] .= 0.0
    wt[1] = 8.0
  elseif nip == 8
    s[1,1] =  root3
    s[1,2] =  root3
    s[1,3] =  root3
    s[2,1] =  root3
    s[2,2] =  root3
    s[2,3] = -root3
    s[3,1] =  root3
    s[3,2] = -root3
    s[3,3] =  root3
    s[4,1] =  root3
    s[4,2] = -root3
    s[4,3] = -root3
    s[5,1] = -root3
    s[5,2] =  root3
    s[5,3] =  root3
    s[6,1] = -root3
    s[6,2] = -root3
    s[6,3] =  root3
    s[7,1] = -root3
    s[7,2] =  root3
    s[7,3] = -root3
    s[8,1] = -root3
    s[8,2] = -root3
    s[8,3] = -root3
    fill!(wt, 1)
  elseif nip == 14
    b = 0.795822426
    c = 0.758786911
    wt[1:6] = 0.886426593
    wt[7:14] = 0.335180055
    s[1,1] = -b
    s[2,1] = b
    s[3,2] = -b
    s[4,2] = b
    s[5,3] = -b
    s[6,3] = b
    s[7:end,:] = c
    s[7,1] = -c
    s[7,2] = -c
    s[7,3] = -c
    s[8,2] = -c
    s[8,3] = -c
    s[9,1] = -c
    s[9,3] = -c
    s[10,3] = -c
    s[11,1] = -c
    s[11,2] = -c
    s[12,2] = -c
    s[13,1] = -c
  elseif nip == 15
    b        = 1.0
    c        = 0.674199862
    wt[1]    = 1.564444444
    wt[2:7]  = 0.355555556
    wt[8:15] = 0.537777778
    s[2,1] = -b
    s[3,1] = b
    s[4,2] = -b
    s[5,2] = b
    s[6,3] = -b
    s[7,3] = b
    s[8:end,:] = c
    s[8,1] = -c
    s[8,2] = -c
    s[8,3] = -c
    s[9,2] = -c
    s[9,3] = -c
    s[10,1] = -c
    s[10,3] = -c
    s[11,3] = -c
    s[12,1] = -c
    s[12,2] = -c
    s[13,2] = -c
    s[14,1] = -c
  elseif nip == 27
    wt[:] = [5.0/9.0*v; 8.0/9.0*v; 5.0/9.0*v]
    s[1:7:3,1] = -r15
    s[2:8:3,1] = 0.0
    s[3:9:3,1] = r15
    s[1:3,3] = r15
    s[4:6,3] = 0.0
    s[7:9,3] = -r15
    s[1:9,2] = -r15
    s[10:16:3,1] = -r15
    s[11:17:3,1] = 0.0
    s[12:18:3,1] = r15
    s[10:12,3] = r15
    s[13:15,3] = 0.0
    s[16:18,3] = -r15
    s[10:18,2] = 0.0
    s[19:25:3,1] = -r15
    s[20:26:3,1] = 0.0
    s[21:27:3,1] = r15
    s[19:21,3] =  r15
    s[22:24,3] = 0.0
    s[25:27,3] = -r15
    s[19:27,2] =  r15
  else
    println("Wrong number of integrating points for a hexaahedron.")
  end
end
