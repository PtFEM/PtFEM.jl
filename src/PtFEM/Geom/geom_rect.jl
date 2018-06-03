"""
## geom_rect!

This subroutine forms the coordinates and connectivity for a
rectangular mesh of right angled triangular elements (3, 6, 10 or 15-node)
or quadrilateral elements (4, 8 or 9-node) counting in the
x- or y-dir. 

### Method
```julia
geom_rect!(fin_el, iel, x_coords, y_coords, coord, num, dir)
```

### Arguments
```julia
* fin_el::FiniteElement            : Shape of finite element
                                     (Trangle or Quadrilateral)
* iel::Int                       : Element number
* x_coords::FloatRange{Float64}    : x coordinates
* y_coords::FloatRange{Float64}    : y coordinates
* coord::Matrix{Float64}           : Nodal coordinates (Updated)
* num::Vector{Int}               : Node numbers (Updated)
* dir::Symbol                      : Node numbering direction
```
"""
function geom_rect!(fin_el::Triangle, iel::Int, x_coords::Array{Float64, 1},
  y_coords::Array{Float64, 1}, coord::Matrix{Float64}, num::Vector{Int}, dir::Symbol)
  
  nxe = size(x_coords, 1) - 1
  nye = (size(y_coords, 1) - 1) * 2
  nod = size(num, 1)
  #println([nxe nye nod])
  if dir == :x || dir == :r
    jel = round.(Int, 2 * nxe * floor((iel - 1) ÷ (2 * nxe)))
    ip = round.(Int, floor((iel - jel + 1) ÷ 2))
    iq = round.(Int, floor(2 * floor(floor((iel - 1) ÷ (2 * nxe)) + 1) - 1 + floor(floor(iel ÷ 2) * 2) / iel))
  else
    #println("Direction != :x or :r.")
    jel = round.(Int, floor((iel - 1) ÷ nye))
    ip = jel + 1
    iq = iel - nye * jel
  end
  #println([jel ip iq])
  if nod == 3
    if mod(iq, 2) != 0
      if dir == :x || dir == :r
        num[1] = (nxe + 1) * round.(Int, (iq - 1) / 2) + ip
        num[2] = num[1] + 1
        num[3] = (nxe + 1) * round.(Int, (iq + 1) / 2) + ip
      else
        num[1] = (ip - 1) * round.(Int, (nye + 2) / 2) + (iq + 1) / 2
        num[2] = num[1] + round.(Int, (nye + 2) / 2)
        num[3] = num[1] + 1
      end
      coord[1, 1] = x_coords[ip]
      coord[1, 2] = y_coords[round.(Int, (iq + 1) / 2)]
      coord[2, 1] = x_coords[ip + 1]
      coord[2, 2] = y_coords[round.(Int, (iq + 1) / 2)]
      coord[3, 1] = x_coords[ip]
      coord[3, 2] = y_coords[round.(Int, (iq + 3) / 2)]
    else
      if dir == :x || dir == :r
        num[1] = (nxe + 1) * round.(Int, iq / 2) + ip + 1
        num[2] = num[1] - 1
        num[3] = (nxe + 1) * round.(Int, (iq - 2) / 2) + ip + 1
      else
        num[1] = ip * round.(Int, (nye + 2) / 2) + round.(Int, (iq + 2) / 2)
        num[2] = (ip - 1) * round.(Int, (nye + 2) / 2) + round.(Int, (iq + 1) / 2 + 1)
        num[3] = num[1] - 1
      end
      coord[1, 1] = x_coords[ip+ 1]
      coord[1, 2] = y_coords[round.(Int, (iq + 2) / 2)]
      coord[2, 1] = x_coords[ip]
      coord[2, 2] = y_coords[round.(Int, (iq + 2) / 2)]
      coord[3, 1] = x_coords[ip + 1]
      coord[3, 2] = y_coords[round.(Int, iq / 2)]
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
     coord[1,2] = y_coords[(iq+1)÷2]
     coord[3,1] = x_coords[ip+1]   
     coord[3,2] = y_coords[(iq+1)÷2]
     coord[5,1] = x_coords[ip]   
     coord[5,2] = y_coords[(iq+3)÷2]
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
      coord[1,2] = y_coords[(iq+2)÷2]
      coord[3,1] = x_coords[ip]   
      coord[3,2] = y_coords[(iq+2)÷2]
      coord[5,1] = x_coords[ip+1] 
      coord[5,2] = y_coords[iq÷2]
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
     coord[1,2] = y_coords[round.(Int, (iq+1)/2)]
     coord[5,1] = x_coords[ip+1]   
     coord[5,2] = y_coords[round.(Int, (iq+1)/2)]
     coord[9,1] = x_coords[ip]   
     coord[9,2] = y_coords[round.(Int, (iq+3)/2)]
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
     coord[1,2] = y_coords[round.(Int, (iq+2)/2)]
     coord[5,1] = x_coords[ip]   
     coord[5,2] = y_coords[round.(Int, (iq+2)/2)]
     coord[9,1] = x_coords[ip+1] 
     coord[9,2] = y_coords[round.(Int, iq/2)]
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
   println("Wrong number of nodes for triangular fin_el.")
   exit(1)
  end
end

function geom_rect!(fin_el::Quadrilateral, iel::Int, x_coords::Array{Float64, 1},
  y_coords::Array{Float64, 1}, coord::Matrix{Float64}, num::Vector{Int}, dir::Symbol)

  nxe = size(x_coords,1)-1
  nye = size(y_coords,1)-1
  nod = size(num, 1)
  if dir == :x || dir == :r
    iq  = round.(Int, floor((iel-1)/nxe))+1
    ip = iel-(iq-1)*nxe
  else
    ip = round.(Int, floor((iel-1)/nye))+1
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
    coord[1:2,1] .= x_coords[ip]
    coord[3:4,1] .= x_coords[ip+1]
    coord[1,2] = y_coords[iq+1]
    coord[2:3,2] .= y_coords[iq]
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
    coord[1:3,1] .= x_coords[ip]
    coord[5:7,1] .= x_coords[ip+1]
    coord[4,1] = 0.5*(coord[3,1]+coord[5,1])
    coord[8,1] = 0.5*(coord[7,1]+coord[1,1])
    coord[1,2] = y_coords[iq+1]
    coord[7:8,2] .= y_coords[iq+1]
    coord[3:5,2] .= y_coords[iq]
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
    println("Wrong number of nodes for quadrilateral fin_el.")
    exit(1)
  end
end