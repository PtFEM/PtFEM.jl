importall Base

### Finite elements used to approximate element ###

abstract Element                 # Finite elements

type Triangle <: Element
  nod::Int64
end

type Quadrilateral <: Element
  nod::Int64
end

type Hexahedron <: Element
  nod::Int64
end

type Axisymmetric <: Element
  nod::Int64
end

### Top level component ###

abstract ElementType                  # Structure element to be modeled

type Plane <: ElementType
  nxe::Int64                      # Number of elements in x direction
  nye::Int64                      # Number of elements in y direction
  nip::Int64                      # Number of integration points
  direction::Symbol               # Node numbering direction
  element::Element               # Finite element type used
end

type Beam3D <: ElementType
  nxe::Int64                      # Number of elements in x direction
  nye::Int64                      # Number of elements in y direction
  nze::Int64                      # Number of elements in z direction
  nip::Int64                      # Number of integration points
  direction::Symbol               # Node numbering direction
  element::Element               # Finite element type used
end

### Model type ###

type FEmodel                      # Computationale data and results structure
  elementtype::ElementType                     # 
  element::Element               #
  
  # Scalars
  ndim::Int64                     # Number of dimensions
  nels::Int64                     # Number of elements
  nst::Int64                      # Number of stress terms
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
  fun::Array{Float64, 1}         # Shape function vector
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
  nodof = 2
  nprops = 2
  penalty = 1e20
  
  elementtype = data[:elementtype]
  element = elementtype.element
  prop = data[:properties]
    
  (nels, nn) = mesh_size(element, element.nod, elementtype.nxe, elementtype.nye)
  
  ndof = element.nod * nodof           # Degrees of freedom per element
  if typeof(element) == Axisymmetric
    nst = 4
  end
  
  # Allocate all arrays
  nf = ones(Int64, nodof, nn)
  for i in 1:size(data[:support], 1)
    nf[:, data[:support][i][1]] = data[:support][i][2]
  end
  
  points = zeros(elementtype.nip, ndim)
  g = zeros(Int64, ndof)
  g_coord = zeros(ndim,nn)
  fun = zeros(element.nod)
  coord = zeros(element.nod, ndim)
  jac = zeros(ndim, ndim)
  g_num = zeros(Int64, element.nod, nels)
  der = zeros(ndim, element.nod)
  deriv = zeros(ndim, element.nod)
  bee = zeros(nst,ndof)
  km = zeros(ndof, ndof)
  eld = zeros(ndof)
  weights = zeros(elementtype.nip)
  g_g = zeros(Int64, ndof, nels)
  num = zeros(Int64, element.nod)
  x_coords = data[:x_coords]
  y_coords = data[:y_coords]
  etype = data[:etype]
  
  etype = ones(Int64, nels)
  if data[:nproperties] > 1
    etype = data[:etype]
  end 
  
  gc = ones(ndim)
  dee = zeros(nst,nst)
  sigma = zeros(nst)
  
  formnf!(nodof, nn, nf)
  neq = maximum(nf)
  kdiag = zeros(Int64, neq)
  
  # Find global array sizes
  
  for iel in 1:nels
    geom_rect!(element, iel, x_coords, y_coords, coord, num, elementtype.direction)
    num_to_g!(num, nf, g)
    g_num[:, iel] = num
    g_coord[:, num] = coord'
    g_g[:, iel] = g
    fkdiag!(kdiag, g)
  end
  
  for i in 2:neq
    kdiag[i] = kdiag[i] + kdiag[i-1]
  end
  
  kv = zeros(kdiag[neq])

  println("There are $(neq) equations and the skyline storage is $(kdiag[neq]).")
  
  sample!(element, points, weights)
  
  for iel in 1:nels
    deemat!(dee, prop[etype[iel], 1], prop[etype[iel], 2])
    num = g_num[:, iel]
    coord = g_coord[:, num]'              # Transpose
    g = g_g[:, iel]
    km = zeros(ndof, ndof)
    for i in 1:elementtype.nip
      shape_fun!(fun, points, i)
      shape_der!(der, points, i)
      jac = der*coord
      detm = det(jac)
      jac = inv(jac)
      deriv = jac*der
      beemat!(bee, deriv)
      if typeof(element) == Axisymmetric
        gc = fun*coord
        bee[4, 1:ndof-1:2] = fun[:]/gc[1]
      end
      km += (bee')*dee*bee*detm*weights[i]*gc[1]
    end
    fsparv!(kv, km, g, kdiag)
  end
  
  loads = zeros(neq + 1)
  for i in 1:size(data[:loaded_nodes], 1)
    loads[nf[:, data[:loaded_nodes][i][1]]+1] = data[:loaded_nodes][i][2]
  end
  
  fixed_freedoms = size(data[:fixed_freedoms], 1)
  
  if fixed_freedoms > 0
    no = zeros(Int64, fixed_freedoms)
    node = zeros(Int64, fixed_freedoms)
    sense = zeros(Int64, fixed_freedoms)
    for i in 1:fixed_freedoms
      no[i] = nf[fixed_freedoms[i][2], fixed_freedoms[i][1]]
    end
    kv[kdiag[no]] = kv[kdiag[no]] + penalty
    loads[no] = kv[kdiag[no]] * value
  end
  
  sparin!(kv, kdiag)
  loads[2:end] = spabac!(kv, loads[2:end], kdiag)
  nf1 = deepcopy(nf) + 1
  
  if typeof(element) == Axisymmetric
    println("\nNode     r-disp          z-disp")
  else
    println("\nNode     x-disp          y-disp")
  end
  
  tmp = []
  for i in 1:nn
    tmp = vcat(tmp, loads[nf1[:,i]])
    xstr = @sprintf("%+.4e", loads[nf1[1,i]])
    ystr = @sprintf("%+.4e", loads[nf1[2,i]])
    println("  $(i)    $(xstr)     $(ystr)")
  end
  #println(round(reshape(float(tmp), 2, 9)', 15))
  
  points = zeros(elementtype.nip, ndim)
  weights = zeros(elementtype.nip)
  sample!(element, points, weights)
  println("\nThe integration point (nip = $(elementtype.nip)) stresses are:")
  if typeof(element) == Axisymmetric
    println("\nElement  r-coord   z-coord     sig_r        sig_z        tau_rz        sig_t")
  else
    println("\nElement  x-coord   y-coord     sig_x        sig_y        tau_xy")
  end
  for iel in 1:nels
    deemat!(dee, prop[etype[iel], 1], prop[etype[iel], 2])
    num = g_num[:, iel]
    coord = g_coord[:, num]'
    g = g_g[:, iel]
    eld = loads[g+1]
    for i in 1:elementtype.nip
      shape_fun!(fun, points, i)
      shape_der!(der, points, i)
      gc = fun'*coord
      jac = inv(der*coord)
      deriv = jac*der
      beemat!(bee, deriv)
      if typeof(element) == Axisymmetric
        gc = fun'*coord
        bee[4, 1:ndof-1:2] = fun[:]/gc[1]
      end
      sigma = dee*(bee*eld)
      gc1 = @sprintf("%+.4f", gc[1])
      gc2 = @sprintf("%+.4f", gc[2])
      s1 = @sprintf("%+.4e", sigma[1])
      s2 = @sprintf("%+.4e", sigma[2])
      s3 = @sprintf("%+.4e", sigma[3])
      println("   $(iel)     $(gc1)   $(gc2)   $(s1)  $(s2)  $(s3)")
    end
  end
  println()
  
  #=
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
