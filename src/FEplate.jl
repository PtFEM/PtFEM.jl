importall Base

### Model type ###

type FEplate
  nels::Int64                     # Number of elements
  nn::Int64                       # Number of nodes in the mesh
  ndim::Int64                     # Number of dimensions
  nod::Int64                      # Number of nodes per element
  nprops::Int64                   # Number of material properties
  np_types::Int64                 # Number of different property types
  nodof::Int64                    # Number of degrees of freedom per node
  ndof::Int64                     # Degrees of freedom per element
  fixed_freedoms::Int64           # Number of fixed displacement
  loaded_nodes::Int64             # Number of loaded nodes

  # Int64 arrays
  etype::Array{Int64, 1}
  g::Array{Int64, 1}
  g_g::Array{Int64, 2}
  g_num::Array{Int64, 2}
  nf::Array{Int64, 2}
  num::Array{Int64, 1}
  data::Dict{Symbol, Any}

  neq::Int64
  kdiag::Array{Int64, 1}

  no::Array{Int64, 1}
  node::Array{Int64, 1}
  sense::Array{Int64, 1}

  # Float64 arrays
  displacements::Array{Float64, 2}
  actions::Array{Float64, 2}
  bee::Array{Float64, 2}
  coord::Array{Float64, 2}
  dee::Array{Float64, 2}
  der::Array{Float64, 2}
  deriv::Array{Float64, 2}
  eld::Array{Float64, 1}
  fun::Array{Function, 1}
  gc::Array{Float64, 1}
  g_coord::Array{Float64, 2}
  jac::Array{Float64, 2}
  km::Array{Float64, 2}
  kv::Array{Float64, 1}
  loads::Array{Float64, 1}
  points::Array{Float64, 2}
  prop::Array{Float64, 2}
  sigma::Array{Float64, 1}
  value::Array{Float64, 1}
  weights::Array{Float64, 1}
  x_coords::Array{Float64, 1}
  y_coords::Array{Float64, 1}
end

function FEplate(nels::Int64, nn::Int64, data::Dict;
   ndim::Int64 = 3, np_types::Int64 = 1, nprops::Int64 = 4, nod::Int64 = 2)
  
  ndim == 2 ? nodof = 3 : nodof = 6   # Degrees of freedom per node
  ndof = nod * nodof                  # Degrees of freedom per element
  fixed_freedoms = 0                  # Number of fixed displacement
  loaded_nodes = 0                    # Number of loaded nodes

  # Int64 arrays
  etype = int(ones(nels))
  g = int(zeros(ndof))
  g_g = int(zeros(ndof, nels))
  g_num = int(zeros(nod, nels))
  nf = int(ones(nodof, nn))
  num = int(zeros(nod))
  
  for i in 1:size(data[:support], 1)
    nf[:, data[:support][i][1]] = data[:support][i][2]
  end
  
  formnf!(nodof, nn, nf)
  neq = maximum(nf)
  kdiag = int(zeros(neq))
  loads = zeros(length(nf))

  for i in 1:size(data[:node_numbering], 1)
    g_num[data[:node_numbering][i][1],:] = data[:node_numbering][i][2]
  end
  
  for i in 1:nels
    num = g_num[:, i]
    num_to_g!(nod, nodof, nn, ndof, num, nf, g)
    g_g[:, i] = g
    fkdiag!(ndof, neq, g, kdiag)
  end
  
  for i in 2:neq
    kdiag[i] = kdiag[i] + kdiag[i-1]
  end
  
  println("There are $(neq) equations and the skyline storage is $(kdiag[neq]).")
  
  no = int(zeros(fixed_freedoms))
  node = int(zeros(fixed_freedoms))
  sense = int(zeros(fixed_freedoms))

  # Float64 arrays
  actions = zeros(ndof, nels)
  coord = zeros(nod, ndim)
  eld = zeros(ndof)
  gamma = zeros(nels)
  g_coord = zeros(ndim, nn)
  km = zeros(ndof, ndof)
  kv = zeros(kdiag[neq])
  loads = zeros(neq)
  prop = zeros(nprops, np_types)

  for i in 1:size(data[:coordinates], 1)
    g_coord[data[:coordinates][i][1],:] = data[:coordinates][i][2]
  end
  
  for i in 1:size(data[:properties], 1)
    prop[:, data[:properties][i][1]] = data[:properties][i][2]
  end
  
  for i in 1:size(data[:loads], 1)
    loads[nf[:, data[:loads][i][1]]] = data[:loads][i][2]
  end
  
  for i in 1:nels
    num = g_num[:, i]
    coord = g_coord[:, num]'              #'
    rigid_jointed!(km, prop, gamma, etype, i, coord)
    g = g_g[:, i]
    fsparv!(kv, km, g, kdiag)
  end
  
  sparin!(kv, kdiag)
  spabac!(kv, loads, kdiag)
  displacements = zeros(size(nf))
  for i in 1:size(displacements, 1)
    for j in 1:size(displacements, 2)
      if nf[i, j] > 0
        displacements[i,j] = loads[nf[i, j]]
      end
    end
  end

  actions = zeros(ndof, nels)
  for i in 1:nels
    num = g_num[:, i]
    coord = g_coord[:, num]'              #'
    g = g_g[:, i]
    eld = zeros(length(g))
    for j in 1:length(g)
      if g[j] != 0
        eld[j] = loads[g[j]]
      end
    end
    rigid_jointed!(km, prop, gamma, etype, i, coord)
    actions[:, i] = km * eld
  end

  FEplate(nels, nn, ndim, nod, nprops, np_types, nodof, ndof,
    fixed_freedoms, loaded_nodes, etype, g, g_g, g_num, nf, num, data,
    neq, kdiag, no, node, sense, displacements, actions, coord, eld,
    gamma, g_coord,km, kv, loads, prop);
end



function model_show(io::IO, m::FEplate, compact::Bool=false)
  if compact==true
    println("FEplate(")
  else
    println("  nels =                    \"$(m.nels)\"")
  end
end

show(io::IO, m::FEplate) = model_show(io, m, false)
showcompact(io::IO, m::FEplate) = model_show(io, m, true)
