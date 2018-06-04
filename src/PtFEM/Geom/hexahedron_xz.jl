"""
## hexahedron_xz!

This subroutine generates nodal coordinates and numbering for
8, 14 or 20-node "bricks" counting x-z planes in the y-direction. 

### Method
```julia
hexahedron_xz!(iel, x_coords, y_coords, z_coords, coord, num)
```

### Arguments
```julia
* iel::Int                       : Element number
* x_coords::FloatRange{Float64}    : x coordinates
* y_coords::FloatRange{Float64}    : y coordinates
* z_coords::FloatRange{Float64}    : y coordinates
* coord::Matrix{Float64}           : Nodal coordinates (Updated)
* num::Vector{Int}               : Node numbers (Updated)
* dir::Symbol                      : Node numbering direction
```
"""
function hexahedron_xz!(iel::Int, x_coords::Vector{Float64}, 
  y_coords::Vector{Float64}, z_coords::Vector{Float64}, 
  coord::LinearAlgebra.Adjoint{Float64,Array{Float64,2}}, num::Vector{Int})
  #
  # This subroutine generates nodal coordinates and numbering for
  # 8, 14 or 20-node "bricks" counting x-z planes in the y-direction.
  #
  # Updates coord, num
  #
  nxe = size(x_coords, 1) - 1
  nze = size(z_coords, 1) - 1
  nod = size(num, 1)
  iq = (iel-1)÷(nxe*nze) + 1
  iplane = iel-(iq-1)*nxe*nze
  is = (iplane-1)÷nxe + 1 
  ip = iplane-(is-1)*nxe
  if nod == 8
   fac1=(nxe+1)*(nze+1)*(iq-1)
   num[1] = fac1+is*(nxe+1) + ip
   num[2] = fac1+(is-1)*(nxe+1) + ip
   num[3] = num[2] + 1
   num[4] = num[1] + 1
   num[5] = (nxe+1)*(nze+1)*iq+is*(nxe+1) + ip
   num[6] = (nxe+1)*(nze+1)*iq+(is-1)*(nxe+1) + ip
   num[7] = num[6] + 1
   num[8] = num[5] + 1

   coord[1:2,1] = x_coords[ip]
   coord[5:6,1] = x_coords[ip]
   coord[3:4,1] = x_coords[ip+1]
   coord[7:8,1] = x_coords[ip+1]

   coord[1:4,2] = y_coords[iq]
   coord[5:8,2] = y_coords[iq+1]

   coord[2:3,3] = z_coords[is]
   coord[6:7,3] = z_coords[is]
   coord[1:3:4,3] = z_coords[is+1]
   coord[5:3:8,3] = z_coords[is+1]

  elseif nod == 14
   fac1=(2*nxe+1)*(2*nze+1)*(iq-1)
   fac2=(2*nxe+1)*(2*nze+1)*iq
   num[1] = fac1+is*(2*nxe+1) + ip
   num[2] = num[1] - (2*nxe+1)
   num[3] = num[2] + 1
   num[4] = num[1] + 1
   num[5] = num[2] + nxe + 1
   num[6] = fac1+(nxe+1)*(nze+1) + nxe*nze+(is-1)*(2*nxe+1) + nxe+ip
   num[7] = num[6] - nxe
   num[8] = num[6] + 1
   num[9] = num[8] + nxe
   num[10] = fac2+is*(2*nxe+1) + ip
   num[11] = num[10] - (2*nxe+1)
   num[12] = num[11] + 1
   num[13] = num[10] + 1
   num[14] = num[11] + nxe + 1
   
   coord[1:2,1] = x_coords[ip]
   coord[6,1] = x_coords[ip]
   coord[10:11,1] = x_coords[ip]
   coord[5:9:2,1] = 0.5*(x_coords[ip] + x_coords[ip+1])
   coord[14,1] = 0.5*(x_coords[ip] + x_coords[ip+1])
   coord[3:4,1] = x_coords[ip+1]
   coord[8,1] = x_coords[ip+1]
   coord[12:13,1] = x_coords[ip+1]

   coord[1:5,2] = y_coords[iq]
   coord[6:9,2] = 0.5*(y_coords[iq] + y_coords[iq+1])
   coord[10:14,2] = y_coords[iq+1]

   coord[2:3,3] = z_coords[is]
   coord[7,3] = z_coords[is]
   coord[11:12,3] = z_coords[is]
   coord[5:6,3] = 0.5*(z_coords[is] + z_coords[is+1])
   coord[8:6:14,3] = 0.5*(z_coords[is] + z_coords[is+1])
   coord[1:4:3,3] = z_coords[is+1]
   coord[9,3] = z_coords[is+1]
   coord[10:3:13,3] = z_coords[is+1]

  elseif nod == 20
   fac1=((2*nxe+1)*(nze+1) + (2*nze+1)*(nxe+1))*(iq-1)
   fac2=((2*nxe+1)*(nze+1) + (2*nze+1)*(nxe+1))*iq
   num[1] = fac1+(3*nxe+2)*is+2*ip-1
   num[2] = fac1+(3*nxe+2)*is-nxe+ip-1
   num[3] = num[1] - 3*nxe-2
   num[4] = num[3] + 1
   num[5] = num[4] + 1
   num[6] = num[2] + 1
   num[7] = num[1] + 2
   num[8] = num[1] + 1
   num[9] = fac2-(nxe+1)*(nze+1) + (nxe+1)*is+ip
   num[10] = num[9]-nxe-1
   num[11] = num[10] + 1
   num[12] = num[9] + 1
   num[13] = fac2+(3*nxe+2)*is+2*ip-1
   num[14] = fac2+(3*nxe+2)*is-nxe+ip-1
   num[15] = num[13] - 3*nxe-2
   num[16] = num[15] + 1
   num[17] = num[16] + 1
   num[18] = num[14] + 1
   num[19] = num[13] + 2
   num[20] = num[13] + 1 

   coord[1:3,1] .= x_coords[ip]
   coord[9:10,1] .= x_coords[ip]
   coord[13:15,1] .= x_coords[ip]
   coord[5:7,1] .= x_coords[ip+1]
   coord[11:12,1] .= x_coords[ip+1]
   coord[17:19,1] .= x_coords[ip+1]
   coord[4:4:8,1] .= 0.5*(x_coords[ip] + x_coords[ip+1])
   coord[16:4:20,1] .= 0.5*(x_coords[ip] + x_coords[ip+1])

   coord[1:8,2] .= y_coords[iq]
   coord[13:20,2] .= y_coords[iq+1]
   coord[9:12,2] .= 0.5*(y_coords[iq] + y_coords[iq+1])

   coord[1,3] = z_coords[is+1]
   coord[7:9,3] .= z_coords[is+1]
   coord[12:13,3] .= z_coords[is+1]
   coord[19:20,3] .= z_coords[is+1]
   coord[3:5,3] .= z_coords[is]
   coord[10:11,3] .= z_coords[is]
   coord[15:17,3] .= z_coords[is]
   coord[2:4:6,3] .= 0.5*(z_coords[is] + z_coords[is+1])
   coord[14:4:18,3] .= 0.5*(z_coords[is] + z_coords[is+1])

  else
    println("Wrong number of nodes for hexahedral fin_el.")
    exit(1)
  end
end

function hexahedron_xz!(iel::Int, x_coords::Vector{Float64}, 
  y_coords::Vector{Float64}, z_coords::Vector{Float64}, 
  coord::Array{Float64,2}, num::Vector{Int})
  #
  # This subroutine generates nodal coordinates and numbering for
  # 8, 14 or 20-node "bricks" counting x-z planes in the y-direction.
  #
  # Updates coord, num
  #
  nxe = size(x_coords, 1) - 1
  nze = size(z_coords, 1) - 1
  nod = size(num, 1)
  iq = (iel-1)÷(nxe*nze) + 1
  iplane = iel-(iq-1)*nxe*nze
  is = (iplane-1)÷nxe + 1 
  ip = iplane-(is-1)*nxe
  if nod == 8
   fac1=(nxe+1)*(nze+1)*(iq-1)
   num[1] = fac1+is*(nxe+1) + ip
   num[2] = fac1+(is-1)*(nxe+1) + ip
   num[3] = num[2] + 1
   num[4] = num[1] + 1
   num[5] = (nxe+1)*(nze+1)*iq+is*(nxe+1) + ip
   num[6] = (nxe+1)*(nze+1)*iq+(is-1)*(nxe+1) + ip
   num[7] = num[6] + 1
   num[8] = num[5] + 1

   coord[1:2,1] = x_coords[ip]
   coord[5:6,1] = x_coords[ip]
   coord[3:4,1] = x_coords[ip+1]
   coord[7:8,1] = x_coords[ip+1]

   coord[1:4,2] = y_coords[iq]
   coord[5:8,2] = y_coords[iq+1]

   coord[2:3,3] = z_coords[is]
   coord[6:7,3] = z_coords[is]
   coord[1:3:4,3] = z_coords[is+1]
   coord[5:3:8,3] = z_coords[is+1]

  elseif nod == 14
   fac1=(2*nxe+1)*(2*nze+1)*(iq-1)
   fac2=(2*nxe+1)*(2*nze+1)*iq
   num[1] = fac1+is*(2*nxe+1) + ip
   num[2] = num[1] - (2*nxe+1)
   num[3] = num[2] + 1
   num[4] = num[1] + 1
   num[5] = num[2] + nxe + 1
   num[6] = fac1+(nxe+1)*(nze+1) + nxe*nze+(is-1)*(2*nxe+1) + nxe+ip
   num[7] = num[6] - nxe
   num[8] = num[6] + 1
   num[9] = num[8] + nxe
   num[10] = fac2+is*(2*nxe+1) + ip
   num[11] = num[10] - (2*nxe+1)
   num[12] = num[11] + 1
   num[13] = num[10] + 1
   num[14] = num[11] + nxe + 1
   
   coord[1:2,1] = x_coords[ip]
   coord[6,1] = x_coords[ip]
   coord[10:11,1] = x_coords[ip]
   coord[5:9:2,1] = 0.5*(x_coords[ip] + x_coords[ip+1])
   coord[14,1] = 0.5*(x_coords[ip] + x_coords[ip+1])
   coord[3:4,1] = x_coords[ip+1]
   coord[8,1] = x_coords[ip+1]
   coord[12:13,1] = x_coords[ip+1]

   coord[1:5,2] = y_coords[iq]
   coord[6:9,2] = 0.5*(y_coords[iq] + y_coords[iq+1])
   coord[10:14,2] = y_coords[iq+1]

   coord[2:3,3] = z_coords[is]
   coord[7,3] = z_coords[is]
   coord[11:12,3] = z_coords[is]
   coord[5:6,3] = 0.5*(z_coords[is] + z_coords[is+1])
   coord[8:6:14,3] = 0.5*(z_coords[is] + z_coords[is+1])
   coord[1:4:3,3] = z_coords[is+1]
   coord[9,3] = z_coords[is+1]
   coord[10:3:13,3] = z_coords[is+1]

  elseif nod == 20
   fac1=((2*nxe+1)*(nze+1) + (2*nze+1)*(nxe+1))*(iq-1)
   fac2=((2*nxe+1)*(nze+1) + (2*nze+1)*(nxe+1))*iq
   num[1] = fac1+(3*nxe+2)*is+2*ip-1
   num[2] = fac1+(3*nxe+2)*is-nxe+ip-1
   num[3] = num[1] - 3*nxe-2
   num[4] = num[3] + 1
   num[5] = num[4] + 1
   num[6] = num[2] + 1
   num[7] = num[1] + 2
   num[8] = num[1] + 1
   num[9] = fac2-(nxe+1)*(nze+1) + (nxe+1)*is+ip
   num[10] = num[9]-nxe-1
   num[11] = num[10] + 1
   num[12] = num[9] + 1
   num[13] = fac2+(3*nxe+2)*is+2*ip-1
   num[14] = fac2+(3*nxe+2)*is-nxe+ip-1
   num[15] = num[13] - 3*nxe-2
   num[16] = num[15] + 1
   num[17] = num[16] + 1
   num[18] = num[14] + 1
   num[19] = num[13] + 2
   num[20] = num[13] + 1 

   coord[1:3,1] .= x_coords[ip]
   coord[9:10,1] .= x_coords[ip]
   coord[13:15,1] .= x_coords[ip]
   coord[5:7,1] .= x_coords[ip+1]
   coord[11:12,1] .= x_coords[ip+1]
   coord[17:19,1] .= x_coords[ip+1]
   coord[4:4:8,1] .= 0.5*(x_coords[ip] + x_coords[ip+1])
   coord[16:4:20,1] .= 0.5*(x_coords[ip] + x_coords[ip+1])

   coord[1:8,2] .= y_coords[iq]
   coord[13:20,2] .= y_coords[iq+1]
   coord[9:12,2] .= 0.5*(y_coords[iq] + y_coords[iq+1])

   coord[1,3] = z_coords[is+1]
   coord[7:9,3] .= z_coords[is+1]
   coord[12:13,3] .= z_coords[is+1]
   coord[19:20,3] .= z_coords[is+1]
   coord[3:5,3] .= z_coords[is]
   coord[10:11,3] .= z_coords[is]
   coord[15:17,3] .= z_coords[is]
   coord[2:4:6,3] .= 0.5*(z_coords[is] + z_coords[is+1])
   coord[14:4:18,3] .= 0.5*(z_coords[is] + z_coords[is+1])

  else
    println("Wrong number of nodes for hexahedral fin_el.")
    exit(1)
  end
end