function geom_rect!(element::Triangle, iel::Int64, x_coords::Array{Float64, 1},
  y_coords::Array{Float64, 1}, coord::Matrix{Float64}, num::Vector{Int64}, dir::Symbol)
  
  nxe = size(x_coords, 1) - 1
  nye = (size(y_coords, 1) - 1) * 2
  nod = size(num, 1)
  #println([nxe nye nod])
  if dir == :x || dir == :z
    jel = int(2 * nxe * floor((iel - 1) / (2 * nxe)))
    ip = int(floor((iel - jel + 1) / 2))
    iq = int(floor(2 * floor(floor((iel - 1) / (2 * nxe)) + 1) - 1 + floor(floor(iel / 2) * 2) / iel))
  else
    #println("Direction != :x or :r.")
    jel = int(floor((iel - 1) / nye))
    ip = jel + 1
    iq = iel - nye * jel
  end
  #println([jel ip iq])
  if nod == 3
    if mod(iq, 2) != 0
      if dir == :x || dir == :z
        num[1] = (nxe + 1) * int((iq - 1) / 2) + ip
        num[2] = num[1] + 1
        num[3] = (nxe + 1) * int((iq + 1) / 2) + ip
      else
        num[1] = (ip - 1) * int((nye + 2) / 2) + (iq + 1) / 2
        num[2] = num[1] + int((nye + 2) / 2)
        num[3] = num[1] + 1
      end
      coord[1, 1] = x_coords[ip]
      coord[1, 2] = y_coords[int((iq + 1) / 2)]
      coord[2, 1] = x_coords[ip + 1]
      coord[2, 2] = y_coords[int((iq + 1) / 2)]
      coord[3, 1] = x_coords[ip]
      coord[3, 2] = y_coords[int((iq + 3) / 2)]
    else
      if dir == :x || dir == :z
        num[1] = (nxe + 1) * int(iq / 2) + ip + 1
        num[2] = num[1] - 1
        num[3] = (nxe + 1) * int((iq - 2) / 2) + ip + 1
      else
        num[1] = ip * int((nye + 2) / 2) + int((iq + 2) / 2)
        num[2] = (ip - 1) * int((nye + 2) / 2) + int((iq + 1) / 2 + 1)
        num[3] = num[1] - 1
      end
      coord[1, 1] = x_coords[ip+ 1]
      coord[1, 2] = y_coords[int((iq + 2) / 2)]
      coord[2, 1] = x_coords[ip]
      coord[2, 2] = y_coords[int((iq + 2) / 2)]
      coord[3, 1] = x_coords[ip + 1]
      coord[3, 2] = y_coords[int(iq / 2)]
    end      
  else
    println("nod != 3 not implemented yet.")
  end
end