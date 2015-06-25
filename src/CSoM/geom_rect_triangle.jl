function geom_rect!(element::Triangle, iel::Int64, x_coords::LinSpace{Float64},
  y_coords::LinSpace{Float64}, coord::Matrix{Float64}, num::Vector{Int64}, dir::Symbol)
  
  nxe = size(x_coords, 1) - 1
  nye = (size(y_coords, 1) - 1) * 2
  nod = size(num, 1)
  #println([nxe nye nod])
  if dir == :x || dir == :r
    jel = round(Int64, 2 * nxe * floor((iel - 1) / (2 * nxe)))
    ip = round(Int64, floor((iel - jel + 1) / 2))
    iq = round(Int64, floor(2 * floor(floor((iel - 1) / (2 * nxe)) + 1) - 1 + floor(floor(iel / 2) * 2) / iel))
  else
    #println("Direction != :x or :r.")
    jel = round(Int64, floor((iel - 1) / nye))
    ip = jel + 1
    iq = iel - nye * jel
  end
  #println([jel ip iq])
  if nod == 3
    if mod(iq, 2) != 0
      if dir == :x || dir == :r
        num[1] = (nxe + 1) * round(Int64, (iq - 1) / 2) + ip
        num[2] = num[1] + 1
        num[3] = (nxe + 1) * round(Int64, (iq + 1) / 2) + ip
      else
        num[1] = (ip - 1) * round(Int64, (nye + 2) / 2) + (iq + 1) / 2
        num[2] = num[1] + round(Int64, (nye + 2) / 2)
        num[3] = num[1] + 1
      end
      coord[1, 1] = x_coords[ip]
      coord[1, 2] = y_coords[round(Int64, (iq + 1) / 2)]
      coord[2, 1] = x_coords[ip + 1]
      coord[2, 2] = y_coords[round(Int64, (iq + 1) / 2)]
      coord[3, 1] = x_coords[ip]
      coord[3, 2] = y_coords[round(Int64, (iq + 3) / 2)]
    else
      if dir == :x || dir == :r
        num[1] = (nxe + 1) * round(Int64, iq / 2) + ip + 1
        num[2] = num[1] - 1
        num[3] = (nxe + 1) * round(Int64, (iq - 2) / 2) + ip + 1
      else
        num[1] = ip * round(Int64, (nye + 2) / 2) + round(Int64, (iq + 2) / 2)
        num[2] = (ip - 1) * round(Int64, (nye + 2) / 2) + round(Int64, (iq + 1) / 2 + 1)
        num[3] = num[1] - 1
      end
      coord[1, 1] = x_coords[ip+ 1]
      coord[1, 2] = y_coords[round(Int64, (iq + 2) / 2)]
      coord[2, 1] = x_coords[ip]
      coord[2, 2] = y_coords[round(Int64, (iq + 2) / 2)]
      coord[3, 1] = x_coords[ip + 1]
      coord[3, 2] = y_coords[round(Int64, iq / 2)]
    end      
  elseif nod == 6
    if mod(iq, 2) !== 0
     if dir == 'x' || dir == 'r'
       num[1] = (iq-1)*(2*nxe+1)+2*ip-1
       num[2] = num[1]+1 
       num[3] = num[1]+2 
       num[4] = (iq-1)*(2*nxe+1)+2*nxe+2*ip+1
       num[5] = (iq+1)*(2*nxe+1)+2*ip-1
       num[6] = num[4]-1 
     else
       num[1] = 2*(nye+1)*(ip-1)+iq
       num[2] = 2*(nye+1)*(ip-1)+nye+1+iq
       num[3] = 2*(nye+1)*ip+iq
       num[4] = num[2]+1
       num[5] = num[1]+2 
       num[6] = num[1]+1
     end
     coord[1,1] = x_coords[ip]
     coord[1,2] = y_coords[(iq+1)/2]
     coord[3,1] = x_coords[ip+1]   
     coord[3,2] = y_coords[(iq+1)/2]
     coord[5,1] = x_coords[ip]   
     coord[5,2] = y_coords[(iq+3)/2]
    else
      if dir == 'x' || dir == 'r'
       num[1] = iq*(2*nxe+1)+2*ip+1
       num[2] = num[1]-1 
       num[3] = num[1]-2 
       num[4] = (iq-2)*(2*nxe+1)+2*nxe+2*ip+1
       num[5] = (iq-2)*(2*nxe+1)+2*ip+1
       num[6] = num[4]+1 
      else 
       num[1] = 2*(nye+1)*ip+iq+1 
       num[2] = 2*(nye+1)*(ip-1)+nye+iq+2
       num[3] = 2*(nye+1)*(ip-1)+iq+1
       num[4] = num[2]-1 
       num[5] = num[1]-2
       num[6] = num[1]-1
      end
      coord[1,1] = x_coords[ip+1]
      coord[1,2] = y_coords[(iq+2)/2]
      coord[3,1] = x_coords[ip]   
      coord[3,2] = y_coords[(iq+2)/2]
      coord[5,1] = x_coords[ip+1] 
      coord[5,2] = y_coords[iq/2]
    end
    coord[2,:] = 0.5*(coord[1,:]+coord[3,:])
    coord[4,:] = 0.5*(coord[3,:]+coord[5,:])
    coord[6,:] = 0.5*(coord[5,:]+coord[1,:])
  elseif nod == 10
    if mod(iq, 2) !== 0
     if dir == 'x' || dir == 'r'
       num[1] = (iq-1)/2*(3*nxe+1)*3+3*ip-2
       num[2] = num[1]+1
       num[3] = num[1]+2
       num[4] = num[1]+3
       num[5] = (iq-1)/2*(3*nxe+1)*3+3*nxe+1+3*ip
       num[6] = (iq-1)/2*(3*nxe+1)*3+6*nxe+2+3*ip-1
       num[7] = (iq-1)/2*(3*nxe+1)*3+9*nxe+3+3*ip-2
       num[8] = num[6]-1
       num[9] = num[5]-2
       num[10] = num[9]+1
     else
       num[1] = (9*(nye-2)/2+12)*(ip-1)+3*(iq-1)/2+1
       num[2] = (9*(nye-2)/2+12)*(ip-1)+3*(nye-2)/2+4+3*(iq-1)/2+1
       num[3] = (9*(nye-2)/2+12)*(ip-1)+3*(nye-2)+8+3*(iq-1)/2+1
       num[4] = (9*(nye-2)/2+12)*(ip-1)+9*(nye-2)/2+12+3*(iq-1)/2+1
       num[5] = num[3]+1 
       num[6] = num[2]+2
       num[7] = num[1]+3
       num[8] = num[1]+2
       num[9] = num[1]+1
       num[10] = num[2]+1
     end
     coord[1,1] = x_coords[ip]
     coord[2,1] = x_coords[ip]+(x_coords[ip+1]-x_coords[ip])/3.0
     coord[3,1] = x_coords[ip]+2.0*(x_coords[ip+1]-x_coords[ip])/3.0
     coord[4,1] = x_coords[ip+1]
     coord[4,2] = y_coords[(iq+1)/2]
     coord[5,2] = y_coords[(iq+1)/2]+(y_coords[(iq+3)/2]-y_coords[(iq+1)/2])/3.0
     coord[6,2] = y_coords[(iq+1)/2]+2.0*(y_coords[(iq+3)/2]-y_coords[(iq+1)/2])/3.0
     coord[7,2] = y_coords[(iq+3)/2]
    else
     if dir == 'x' || dir == 'r'
       num[1] = (iq-2)/2*(3*nxe+1)*3+9*nxe+3+3*ip+1
       num[2] = num[1]-1
       num[3] = num[1]-2
       num[4] = num[1]-3
       num[5] = (iq-2)/2*(3*nxe+1)*3+6*nxe+2+3*ip-1
       num[6] = (iq-2)/2*(3*nxe+1)*3+3*nxe+1+3*ip
       num[7] = (iq-2)/2*(3*nxe+1)*3+3*ip+1
       num[8] = num[6]+1
       num[9] = num[5]+2
       num[10] = num[9]-1
     else
       num[1] = (9*(nye-2)/2+12)*(ip-1)+9*(nye-2)/2+12+3*iq/2+1
       num[2] = (9*(nye-2)/2+12)*(ip-1)+3*(nye-2)+8+3*iq/2+1
       num[3] = (9*(nye-2)/2+12)*(ip-1)+3*(nye-2)/2+4+3*iq/2+1
       num[4] = (9*(nye-2)/2+12)*(ip-1)+3*iq/2+1
       num[5] = num[3]-1
       num[6] = num[2]-2
       num[7] = num[1]-3
       num[8] = num[1]-2
       num[9] = num[1]-1
       num[10] = num[2]-1
     end
     coord[1,1] = x_coords[ip+1]
     coord[2,1] = x_coords[ip+1]-(x_coords[ip+1]-x_coords[ip])/3.0
     coord[3,1] = x_coords[ip+1]-2.0*(x_coords[ip+1]-x_coords[ip])/3.0
     coord[4,1] = x_coords[ip]
     coord[4,2] = y_coords[(iq+2)/2]
     coord[5,2] = y_coords[(iq+2)/2]-(y_coords[(iq+2)/2]-y_coords[iq/2])/3.0
     coord[6,2] = y_coords[(iq+2)/2]-2.0*(y_coords[(iq+2)/2]-y_coords[iq/2])/3.0
     coord[7,2] =y_coords[iq/2]
    end
    coord[5,1] = coord[3,1]
    coord[6,1] = coord[2,1]
    coord[7,1] = coord[1,1]
    coord[8,1] = coord[1,1]
    coord[9,1] = coord[1,1]
    coord[10,1] = coord[2,1]
    coord[1,2] = coord[4,2]
    coord[2,2] = coord[4,2]
    coord[3,2] = coord[4,2]
    coord[8,2] = coord[6,2]
    coord[9,2] = coord[5,2]
    coord[10,2] = coord[5,2]
  elseif nod == 15
   if mod(iq, 2) !== 0
     if dir == 'x' || dir == 'r'
       fac1 = 4*(4*nxe+1)*(iq-1)/2
       num[1] = fac1+4*ip-3
       num[2] = num[1]+1
       num[3] = num[1]+2
       num[4] = num[1]+3
       num[5] = num[1]+4
       num[6] = fac1+ 4*nxe+1+4*ip
       num[7] = fac1+ 8*nxe+1+4*ip
       num[8] = fac1+12*nxe+1+4*ip
       num[9] = fac1+16*nxe+1+4*ip
       num[10] = num[8]-1
       num[11] = num[7]-2
       num[12] = num[6]-3
       num[13] = num[12]+1
       num[14] = num[12]+2
       num[15] = num[11]+1
     else
       fac1=4*(2*nye+1)*(ip-1)+2*iq-1 
       num[1] = fac1
       num[2] = fac1+2*nye+1
       num[3] = fac1+4*nye+2 
       num[4] = fac1+6*nye+3 
       num[5] = fac1+8*nye+4
       num[6] = fac1+6*nye+4 
       num[7] = fac1+4*nye+4 
       num[8] = fac1+2*nye+4
       num[9] = fac1+4 
       num[10] = fac1+3 
       num[11] = fac1+2 
       num[12] = fac1+1
       num[13] = fac1+2*nye+2 
       num[14] = fac1+4*nye+3
       num[15] = fac1+2*nye+3  
     end
     coord[1,1] = x_coords[ip]
     coord[1,2] = y_coords[round(Int64, (iq+1)/2)]
     coord[5,1] = x_coords[ip+1]   
     coord[5,2] = y_coords[round(Int64, (iq+1)/2)]
     coord[9,1] = x_coords[ip]   
     coord[9,2] = y_coords[round(Int64, (iq+3)/2)]
   else
     if dir == 'x' || dir == 'r'
       fac1=4*(4*nxe+1)*(iq-2)/2
       num[1] = fac1+16*nxe+5+4*ip
       num[2] = num[1]-1
       num[3] = num[1]-2
       num[4] = num[1]-3
       num[5] = num[1]-4
       num[6] = fac1+12*nxe+1+4*ip
       num[7] = fac1+8*nxe+1+4*ip
       num[8] = fac1+4*nxe+1+4*ip
       num[9] = fac1+4*ip+1
       num[10] = num[8]+1
       num[11] = num[7]+2
       num[12] = num[6]+3
       num[13] = num[12]-1
       num[14] = num[12]-2
       num[15] = num[11]-1
     else
       fac1=4*(2*nye+1)*(ip-1)+2*iq+8*nye+5 
       num[1] = fac1 
       num[2] = fac1-2*nye-1
       num[3] = fac1-4*nye-2 
       num[4] = fac1-6*nye-3 
       num[5] = fac1-8*nye-4
       num[6] = fac1-6*nye-4  
       num[7] = fac1-4*nye-4 
       num[8] = fac1-2*nye-4
       num[9] = fac1-4
       num[10] = fac1-3 
       num[11] = fac1-2 
       num[12] = fac1-1
       num[13] = fac1-2*nye-2  
       num[14] = fac1-4*nye-3
       num[15] = fac1-2*nye-3 
     end
     coord[1,1] = x_coords[ip+1]
     coord[1,2] = y_coords[round(Int64, (iq+2)/2)]
     coord[5,1] = x_coords[ip]   
     coord[5,2] = y_coords[round(Int64, (iq+2)/2)]
     coord[9,1] = x_coords[ip+1] 
     coord[9,2] = y_coords[round(Int64, iq/2)]
   end
   coord[3,:] = 0.5*(coord[1,:]+coord[5,:])
   coord[7,:] = 0.5*(coord[5,:]+coord[9,:])
   coord[11,:] = 0.5*(coord[9,:]+coord[1,:])
   coord[2,:] = 0.5*(coord[1,:]+coord[3,:])
   coord[4,:] = 0.5*(coord[3,:]+coord[5,:])
   coord[6,:] = 0.5*(coord[5,:]+coord[7,:])
   coord[8,:] = 0.5*(coord[7,:]+coord[9,:])
   coord[10,:] = 0.5*(coord[9,:]+coord[11,:])
   coord[12,:] = 0.5*(coord[11,:]+coord[1,:])
   coord[15,:] = 0.5*(coord[7,:]+coord[11,:])
   coord[14,:] = 0.5*(coord[3,:]+coord[7,:])
   coord[13,:] = 0.5*(coord[2,:]+coord[15,:])
  else
   println("Wrong number of nodes for triangular element.")
   exit(1)
  end
end
