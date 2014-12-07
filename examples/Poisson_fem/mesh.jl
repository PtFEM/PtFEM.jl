# poisson_fem.jl
#
# Functions related to mesh generation
#
# Amuthan Arunkumar Ramabathiran
# (aparyap@gmail.com)
#
# Distributed under The Code Project Open License (CPOL)
# http://www.codeproject.com/info/cpol10.aspx

# Data structure to store mesh
type Mesh
  dim  :: Int

  n_el :: Int
  n_nd :: Int

  x_node   :: Array{Float64, 2}
  topology :: Array{Int, 2}

  Mesh(dim::Int, n_el::Int, n_nd::Int, x_node::Array{Float64,2}, topology::Array{Float64,2}) = new(dim, n_el, n_nd, x_node, topology)
end

# Triangulation of the unit square
function UnitSquare(n::Int)
  dim = 2

  n_el = 2n^2
  n_nd = (n+1)^2

  x_node = zeros(n_nd,2)
  n_count = 0
  for j = 1:(n+1)
    yj = (j-1)/n
    for i = 1:(n+1)
      xi = (i-1)/n
      n_count += 1
      x_node[n_count,1] = xi
      x_node[n_count,2] = yj
    end
  end

  topology = zeros(n_el,3)
  e_count = 0
  for j = 1:n
    for i = 1:n
      e_count += 1
      topology[e_count,1] = (j-1)*(n+1) + i
      topology[e_count,2] = (j-1)*(n+1) + i + 1
      topology[e_count,3] = j*(n+1) + i + 1

      e_count += 1
      topology[e_count,1] = (j-1)*(n+1) + i
      topology[e_count,2] = j*(n+1) + i + 1
      topology[e_count,3] = j*(n+1) + i
    end
  end

  mesh = Mesh(dim, n_el, n_nd, x_node, topology)
  return mesh
end

# Write mesh node and connectivity data to file
function write_to_file(mesh::Mesh)
  fnode = open("nodes.dat","w")
  for i = 1:mesh.n_nd
    print(fnode, i, "\t")
    for j = 1:mesh.dim
      print(fnode, mesh.x_node[i,j], "\t")
    end
    print(fnode, "\n")
  end
  close(fnode)

  felt = open("elements.dat","w")
  for i = 1:mesh.n_el
    print(felt, i, "\t")
    for j = 1:3
      print(felt, mesh.topology[i,j], "\t")
    end
    print(felt,"\n")
  end
  close(felt)
end
