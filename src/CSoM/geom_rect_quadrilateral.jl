function geom_rect!(element::Quadrilateral, iel::Int64, x_coords::Array{Float64, 1},
  y_coords::Array{Float64, 1}, coord::Matrix{Float64}, num::Vector{Int64}, dir::Symbol)

  nxe = size(x_coords,1)-1
  nye = size(y_coords,1)-1
  nod = size(num, 1)
  if dir == :x || dir == :r
    iq  = int(floor((iel-1)/nxe))+1
    ip = iel-(iq-1)*nxe
  else
    ip = int(floor((iel-1)/nye))+1
    iq = iel-(ip-1)*nye
  end
  if nod == 4
    if dir == :x || dir == :r
     num[1] = iq*(nxe+1)+ip		        		
     num[2] = (iq-1)*(nxe+1)+ip				
     num[3] = num[2]+1					
     num[4] = num[1]+1					
    else
     num[1] = (ip-1)*(nye+1)+iq+1
     num[2] = num[1]-1
     num[3] = ip*(nye+1)+iq
     num[4] = num[3]+1
    end
    coord[1:2,1] = x_coords[ip]
    coord[3:4,1] = x_coords[ip+1]
    coord[1,2] = y_coords[iq+1]
    coord[2:3,2] = y_coords[iq]
    coord[4,2] = coord[1,2]
  elseif nod == 5
    if dir == :x || dir == :r
     num[1] = iq*(2*nxe+1)+ip
     num[2] = (iq-1)*(2*nxe+1)+ip
     num[3] = num[2]+1
     num[4] = num[1]+1
     num[5] = iq*(2*nxe+1)+ip-nxe
    else
     num[1] = (ip-1)*(2*nye+1)+iq+1
     num[2] = num[1]-1
     num[3] = ip*(2*nye+1)+iq
     num[4] = num[3]+1
     num[5] = ip*(2*nye+1)+iq-nye
    end
    coord[1:2,1] = x_coords[ip]
    coord[3:4,1] = x_coords[ip+1]
    coord[1,2] = y_coords[iq+1]
    coord[2:3,2] = y_coords[iq]
    coord[4,2] = coord[1,2]
    coord[5,:] = 0.25_iwp*(coord[1,:]+coord[2,:]+coord[3,:]+coord[4,:])
  elseif nod == 8
    if dir == :x || dir == :r
     num[1] = iq*(3*nxe+2)+2*ip-1                 
     num[2] = iq*(3*nxe+2)+ip-nxe-1		  
     num[3] = (iq-1)*(3*nxe+2)+2*ip-1		   
     num[4] = num[3]+1
     num[5] = num[4]+1
     num[6] = num[2]+1
     num[7] = num[1]+2
     num[8] = num[1]+1
    else
     num[1] = (ip-1)*(3*nye+2)+2*iq+1
     num[2] = num[1]-1
     num[3] = num[1]-2
     num[4] = (ip-1)*(3*nye+2)+2*nye+iq+1
     num[5] = ip*(3*nye+2)+2*iq-1
     num[6] = num[5]+1
     num[7] = num[5]+2
     num[8] = num[4]+1
    end
    coord[1:3,1] = x_coords[ip]
    coord[5:7,1] = x_coords[ip+1]
    coord[4,1] = 0.5*(coord[3,1]+coord[5,1])
    coord[8,1] = 0.5*(coord[7,1]+coord[1,1])
    coord[1,2] = y_coords[iq+1]
    coord[7:8,2] = y_coords[iq+1]
    coord[3:5,2] = y_coords[iq]
    coord[2,2] = 0.5*(coord[1,2]+coord[3,2])
    coord[6,2] = 0.5*(coord[5,2]+coord[7,2])
  elseif nod == 9
    if dir == :x || dir == :r
     num[1] = iq*(4*nxe+2)+2*ip-1
     num[2] = iq*(4*nxe+2)+2*ip-nxe-4
     num[3] =  (iq-1)*(4*nxe+2)+2*ip-1
     num[4] = num[3]+1
     num[5] = num[4]+1
     num[6] = num[2]+2
     num[7] = num[1]+2
     num[8] = num[1]+1
     num[9] = num[2]+1
    else
     num[1] = (ip-1)*2*(2*nye+1)+2*iq+1
     num[2] = num[1]-1
     num[3] = num[1]-2
     num[4] = (ip-1)*2*(2*nye+1)+2*nye+2*iq
     num[5] = ip*2*(2*nye+1)+2*iq-1
     num[6] = num[5]+1
     num[7] = num[5]+2
     num[8] = num[4]+2
     num[9] = num[4]+1
    end
    coord[1:3,1] = x_coords[ip]
    coord[5:7,1] = x_coords[ip+1]
    coord[4,1] = 0.5*(coord[3,1]+coord[5,1])
    coord[8,1] = 0.5*(coord[7,1]+coord[1,1])
    coord[1,2] = y_coords[iq+1]
    coord[7:8,2] = y_coords[iq+1]
    coord[3:5,2] = y_coords[iq]
    coord[2,2] = 0.5*(coord[1,2]+coord[3,2])
    coord[6,2] = 0.5*(coord[5,2]+coord[7,2])
    coord[9,:] = 0.5*(coord[4,:]+coord[8,:])
  else
    println("Wrong number of nodes for quadrilateral element.")
    exit(1)
  end
end