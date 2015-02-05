importall Base

### Finite elements used to approximate element ###

abstract FiniteElement                 # Finite elements

type Triangle <: FiniteElement
  nod::Int64
end

type Quadrilateral <: FiniteElement
  nod::Int64
end

type Hexahedron <: FiniteElement
  nod::Int64
end

type Axisymmetric <: FiniteElement
  nod::Int64
end

### Top level component ###

abstract Element                  # Structure element to be modeled

type Plane <: FiniteElement
  nxe::Int64                      # Number of elements in x direction
  nye::Int64                      # Number of elements in y direction
  direction::Symbol               # Node numbering direction
  fe::FiniteElement               # Finite element type used
end

type Beam3D <: FiniteElement
  nxe::Int64                      # Number of elements in x direction
  nye::Int64                      # Number of elements in y direction
  nze::Int64                      # Number of elements in z direction
  direction::Symbol               # Node numbering direction
  fe::FiniteElement               # Finite element type used
end

### Model type ###

type FEmodel                      # Computationale data and results structure
  el::Element                     # 
  fe::FiniteElement               #
  
  # Scalars
  ndim::Int64                     # Number of dimensions
  
  nels::Int64                     # Number of elements
  nxe::Int64                      # Number of elements in x direction
  nye::Int64                      # Number of elements in y direction
  nze::Int64                      # Number of elements in z direction
  nst::Int64                      # Number of stress terms
  
  nod::Int64                      # Number of nodes per element
  ndof::Int64                     # Degrees of freedom per element
  nip::Int64                      # Number of integrations points per element

  nn::Int64                       # Number of nodes in the mesh
  nodof::Int64                    # Number of degrees of freedom per node
  fixed_freedoms::Int64           # Number of fixed node displacements
  loaded_nodes::Int64             # Number of loaded nodes
  nr::Int64                       # Number of restrained nodes
  
  nprops::Int64                   # Number of material properties
  np_types::Int64                 # Number of different property types
  
  neq::Int64                      # Number of equations
  penalty::Float64                # Penalty for fixed nodes

  # Int64 arrays
  etype::Array{Int64, 1}          # Element property vector
  g::Array{Int64, 1}              # Element steering vector
  g_g::Array{Int64, 2}            # Global element steering matrix
  g_num::Array{Int64, 2}          # Global element node numbers matrix
  kdiag::Array{Int64, 1}          # Diagonal term location vector
  nf::Array{Int64, 2}             # Nodal freedom matrix
  no::Array{Int64, 1}             # Fixed freedom number vector
  node::Array{Int64, 1}           # Fixed nodes vector
  num::Array{Int64, 1}            # Element node number vector
  sense::Array{Int64, 1}          # Sense of freedoms to be fixed vector

  # Float64 arrays
  actions::Array{Float64, 2}      # Actions
  bee::Array{Float64, 2}          # Strain displacement matrix
  coord::Array{Float64, 2}        # Element nodal coordinates
  dee::Array{Float64, 2}          # Stress strain matrix
  der::Array{Float64, 2}          # Shape function derivatives w.r.t. local coord
  deriv::Array{Float64, 2}        # Shape function derivatives w.r.t. global coord
  displacements::Array{Float64, 2}# Array of node displacements
  eld::Array{Float64, 1}          # Elemental node displacements
  fun::Array{Function, 1}         # Shape function vector
  gc::Array{Float64, 1}           # Integrating point coordinates
  g_coord::Array{Float64, 2}      # Global node coordinates
  jac::Array{Float64, 2}          # Jacobian matrix
  km::Array{Float64, 2}           # Element stiffness matrix
  kv::Array{Float64, 1}           # Global stiffness matrix (skyline vector)
  loads::Array{Float64, 1}        # Nodel loads and displacements
  points::Array{Float64, 2}       # Integrating point lacal coordinates
  prop::Array{Float64, 2}         # Element properties (E and nu for each element)
  sigma::Array{Float64, 1}        # Stress terms
  value::Array{Float64, 1}        # Fixed values for displacements
  weights::Array{Float64, 1}      # Weighting coefficients
  x_coords::Array{Float64, 1}     # x(r)-coordinates of mesh layout
  y_coords::Array{Float64, 1}     # y(z)-coordinates of mesh layout
end

function FEmodel(data::Dict)
  
  ndim = 2
  nst = 3
  nodof =2
  nprops = 2
  penalty = 1e20
  
  el = data[:element]
  fe = el.fe 
  (nels, nn) = mesh_size(fe, fe.nod, el.nxe, el.nye)
  
  ndof = fe.nod * nodof           # Degrees of freedom per element
  if typeof(fe) == Axisymmetric
    nst = 4
  end
  # Int64 arrays
  etype = int(ones(nels))
  g = int(zeros(ndof))
  g_g = int(zeros(ndof, nels))
  g_num = int(zeros(fe.nod, nels))
  nf = int(ones(nodof, nn))
  num = int(zeros(fe.nod))
  
  for i in 1:size(data[:support], 1)
    nf[:, data[:support][i][1]] = data[:support][i][2]
  end
  
  formnf!(nodof, nn, nf)
  neq = maximum(nf)
  kdiag = int(zeros(neq))
  loads = zeros(length(nf))

  #=
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
  
  FEmodel(nels, nn, ndim, nod, nprops, np_types, nodof, ndof,
    fixed_freedoms, loaded_nodes, etype, g, g_g, g_num, nf, num, data,
    neq, kdiag, no, node, sense, displacements, actions, coord, eld,
    gamma, g_coord,km, kv, loads, prop);
  =#
end


function model_show(io::IO, m::FEmodel, compact::Bool=false)
  if compact==true
    println("FEmodel(")
  else
    println("  nels =                    \"$(m.nels)\"")
  end
end

show(io::IO, m::FEmodel) = model_show(io, m, false)
showcompact(io::IO, m::FEmodel) = model_show(io, m, true)
