import Base.show

### Model type ###

immutable FEM                     # Computationale data and results structure
  struc_el::StructuralElement       # Store the fin_el type object
  fin_el::FiniteElement                # Store finite element object
  
  # Scalars
  ndim::Int64                     # Number of dimensions
  nels::Int64                     # Number of fin_els
  nst::Int64                      # Number of stress terms
  ndof::Int64                     # Degrees of freedom per fin_el
  nn::Int64                       # Number of nodes in the mesh
  nodof::Int64                    # Number of degrees of freedom per node

  neq::Int64                      # Number of equations
  penalty::Float64                # Penalty for fixed nodes

  # Int64 arrays
  etype::Array{Int64, 1}          # Element property vector
  g::Array{Int64, 1}              # Element steering vector
  g_g::Array{Int64, 2}            # Global fin_el steering matrix
  g_num::Array{Int64, 2}          # Global fin_el node numbers matrix
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
  gamma::Vector{Float64}          # Rotation vector around local x axis
  dee::Array{Float64, 2}          # Stress strain matrix
  
  der::Array{Float64, 2}          # Shape function derivatives w.r.t. local coord
  deriv::Array{Float64, 2}        # Shape function derivatives w.r.t. global coord
  displacements::Array{Float64, 2}# Array of node displacements
  eld::Array{Float64, 1}          # Elemental node displacements
  fun::Array{Float64, 1}          # Shape function vector
  
  gc::Array{Float64, 2}           # Integrating point coordinates
  g_coord::Array{Float64, 2}      # Global node coordinates
  jac::Array{Float64, 2}          # Jacobian matrix
  km::Array{Float64, 2}           # Element stiffness matrix
  mm::Array{Float64, 2}           # Element mass matrix
  kg::Array{Float64, 2}           # Element geometric matrix
  kv::Array{Float64, 1}           # Global stiffness matrix (skyline vector)
  gv::Array{Float64, 1}           # Global geometric matrix (skyline vector)
  
  loads::Array{Float64, 1}        # Nodel loads and displacements
  points::Array{Float64, 2}       # Integrating point lacal coordinates
  prop::Array{Float64, 2}         # Element properties (E and nu for each fin_el)
  sigma::Array{Float64, 1}        # Stress terms
  value::Array{Float64, 1}        # Fixed values for displacements
  
  weights::Array{Float64, 1}      # Weighting coefficients
  x_coords::Array{Float64, 1}     # x(r)-coordinates of mesh layout
  y_coords::Array{Float64, 1}     # y(z)-coordinates of mesh layout
  z_coords::Array{Float64, 1}     # y-coordinates of mesh layout
  axial::Array{Float64, 1}        # Axial force
end

#=
function model_show(io::IO, m::FEM, compact::Bool=false)
  if compact==true
    println("FEmodel(")
  else
    println("  nels =                    \"$(m.nels)\"")
  end
end

show(io::IO, m::FEM) = model_show(io, m, false)
showcompact(io::IO, m::FEM) = model_show(io, m, true)
=#