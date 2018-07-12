"""
# Method p41 

One dimensional analysis of an axially loaded elastic Rod using 2-node 
Line elements. 

### Constructors
```julia
p41(data)
```
### Arguments
```julia
* `data::Dict{Symbol, Any}`  : Dictionary containing all input data
```

### Required data dictionary keys
```julia
* struc_el::StructuralElement                          : Type of  structural fin_el
* support::Array{Tuple{Int,Array{Int,1}},1}        : Fixed-displacements vector
* loaded_nodes::Array{Tuple{Int,Array{Float64,1}},1} : Node load vector
* properties::Vector{Float64}                          : Material properties
* x_coords::FloatRange{Float64}                        : x-coordinate vector
```

### Optional additional data dictionary keys
```julia
* penalty = 1e20               : Penalty used for fixed degrees of freedoms
* etype::Vector{Int}         : Element material vector if np_types > 1
* eq_nodal_forces_and_moments  : Contribution of distributed loads to loaded_nodes
```

### Return values
```julia
* (jfem, dis_df, fm_df)        : Tuple of jFem, dis_df and fm_df
                                 where:
                                    jfem::jFem    : Computational result type
                                    dis_df        : Displacement data table
                                    fm_df         : Forces and moments data table
```


### Related help
```julia
?StructuralElement             : List of available structural element types
?Rod                           : Help on a Rod structural element
?FiniteElement                 : List finite element types
?Line                          : Help on Line finite element
```
"""
function p41(data::Dict{Symbol, Any})
  
  # Parse & check FE problem data input dict
  
  if :struc_el in keys(data)
    struc_el = data[:struc_el]
  else
    throw("No structural element type specified.")
  end
  
  if typeof(struc_el) == PtFEM.Rod
    ndim = 1
    nst = struc_el.np_types
  else
    throw("p41 expects a Rod structural element.")
  end
  
  fin_el = struc_el.fin_el
  @assert typeof(fin_el) <: FiniteElement
  
  if typeof(fin_el) == Line
    (nels, nn) = PtFEM.mesh_size(fin_el, struc_el.nxe)
  else
    throw("p41 expects a Line (Interval) finite element.")
  end
   
  nodof = fin_el.nodof         # Degrees of freedom per node
  ndof = fin_el.nod * nodof    # Degrees of freedom per fin_el
  
  # Set penalty
  
  penalty = :penalty in keys(data) ? data[:penalty] : 1e20
  
  # All dynamic arrays
  
  points = zeros(struc_el.nip, ndim)   # 
  g = zeros(Int, ndof)                   # Element steering vector
  g_coord = zeros(ndim,nn)                 # 
  fun = zeros(fin_el.nod)                 #
  coord = zeros(fin_el.nod, ndim)         #
  gamma = zeros(nels)                      #
  jac = zeros(ndim, ndim)                  #
  g_num = zeros(Int, fin_el.nod, nels)  # 
  der = zeros(ndim, fin_el.nod)           #
  deriv = zeros(ndim, fin_el.nod)         #
  bee = zeros(nst,ndof)                    #
  km = zeros(ndof, ndof)                   #
  mm = zeros(ndof, ndof)                   #
  gm = zeros(ndof, ndof)                   #
  kg = zeros(ndof, ndof)                   #
  eld = zeros(ndof)                        #
  weights = zeros(struc_el.nip)        #
  g_g = zeros(Int, ndof, nels)           #
  num = zeros(Int, fin_el.nod)          #
  actions = zeros(nels, ndof)              #
  nf = ones(Int, nodof, nn)              #
  displacements = zeros(size(nf, 1), ndim) #
  gc = ones(ndim, ndim)                    #
  dee = zeros(nst,nst)                     #
  sigma = zeros(nst)                       #
  axial = zeros(nels)                      #
  x_coords = zeros(nn)                     #
  y_coords = zeros(nn)                     # Not used, needed for FEM constructor
  z_coords = zeros(nn)                     #
  etype = ones(Int, nels)                #
  ell = zeros(nels)

  # Start with arrays to be initialized from input dict
  
  if :properties in keys(data)
    prop = zeros(size(data[:properties], 1), size(data[:properties], 2))
    for i in 1:size(data[:properties], 1)
      prop[i, :] = data[:properties][i, :]
    end
  else
    throw("No :properties key found in input dict to FE4_1.")
  end
  
  # Update nodal freedom array from input dict :support entry
  # nf has dimensions [nodof, nn] and all entries are set to 1 (free)
  
  if :support in keys(data)
    for i in 1:size(data[:support], 1)
      nf[:, data[:support][i][1]] = data[:support][i][2]
    end
  else
    throw("No :support key found in input dict to FE4_1.")
  end

  if :x_coords in keys(data)
    x_coords = data[:x_coords]
    @assert length(x_coords) == nels + 1
  else
    dl = 1.0/nels
    x_coords = 0.0:dl:1.0
  end
  
  # Set lengths of elements
  for i in 1:length(x_coords)-1
    ell[i] = x_coords[i+1] - x_coords[i]
  end

  # In multiple np_types, get np_type for each structural element
  if :etype in keys(data)
    etype = data[:etype]
  end
  
  # Done with input dict and allocations, re-do nf (assign number to each dof)
  
  PtFEM.formnf!(nodof, nn, nf)
  neq = maximum(nf)
  #kdiag = zeros(Int, neq)
  
  # Set global numbering, coordinates and array sizes
  
  for i in 1:nels
    num = [i; i+1]
    PtFEM.num_to_g!(num, nf, g)
    g_g[:, i] = g
  end
  
  println("There are $(neq) equations.")
    
  loads = OffsetArray(zeros(neq+1), 0:neq)
  if :loaded_nodes in keys(data)
    for i in 1:size(data[:loaded_nodes], 1)
      loads[nf[:, data[:loaded_nodes][i][1]]] = data[:loaded_nodes][i][2]
    end
  end
  
  gsm = spzeros(neq, neq)
  for i in 1:nels
    km = PtFEM.rod_km!(km, prop[etype[i], 1], ell[i])
    g = g_g[:, i]
    PtFEM.fsparm!(gsm, g, km)
  end

  fixed_freedoms = 0
  if :fixed_freedoms in keys(data)
    fixed_freedoms = size(data[:fixed_freedoms], 1)
  end
  no = zeros(Int, fixed_freedoms)
  node = zeros(Int, fixed_freedoms)
  sense = zeros(Int, fixed_freedoms)
  value = zeros(Float64, fixed_freedoms)
  if :fixed_freedoms in keys(data) && fixed_freedoms > 0
    for i in 1:fixed_freedoms
      node[i] = data[:fixed_freedoms][i][1]
      sense[i] = data[:fixed_freedoms][i][2]
      no[i] = nf[sense[i], node[i]]
      value[i] = data[:fixed_freedoms][i][3]
      gsm[no[i], no[i]] += penalty
      loads[no[i]] = gsm[no[i], no[i]] .* value[i]
    end
  end
  
  cfgsm = cholesky(gsm)
  
  loads[1:neq] = cfgsm \ loads[1:neq]
  println()

  displacements = zeros(size(nf))
  for i in 1:size(displacements, 1)
    for j in 1:size(displacements, 2)
      if nf[i, j] > 0
        displacements[i,j] = loads[nf[i, j]]
      end
    end
  end
  displacements = displacements'

  loads[0] = 0.0
  for i in 1:nels
    km = PtFEM.rod_km!(km, prop[etype[i], 1], ell[i])
    g = g_g[:, i]
    eld = loads[g]
    actions[i, :] = km * eld
  end

  
  dis_df = DataFrame(
    x_translation = displacements[:, 1],
  )

  if :eq_nodal_forces_and_moments in keys(data)
    fm_df = DataFrame(
      normal_force_1 = actions[:, 1],
      normal_force_2 = actions[:, 2],
      nf_1_uncorrected = actions[:, 1],
      nf_2_uncorrected = actions[:, 2]
    )
  else
    fm_df = DataFrame(
      normal_force_1 = actions[:, 1],
      normal_force_2 = actions[:, 2]
    )
  end
  
  # Correct element forces and moments for equivalent nodal
  # forces and moments introduced for loading between nodes
  if :eq_nodal_forces_and_moments in keys(data)
    eqfm = data[:eq_nodal_forces_and_moments]
    k = data[:struc_el].fin_el.nod * data[:struc_el].fin_el.nodof
    for t in eqfm
      vals = convert(Array, fm_df[t[1], :])
      for i in 1:k
        fm_df[t[1], i] = round.(vals[i] - t[2][i], digits=2)
      end
    end
  end
  
  fem = PtFEM.jFEM(struc_el, fin_el, ndim, nels, nst, ndof, nn, nodof,
    neq, penalty, etype, g, g_g, g_num, nf, no,
    node, num, sense, actions, bee, coord, gamma, dee,
    der, deriv, displacements, eld, fun, gc, g_coord, jac,
    km, mm, kg, cfgsm, loads, points, prop, sigma, value,
    weights, x_coords, y_coords, z_coords, axial)

  (fem, dis_df, fm_df)
end


"""
# Method p41 

One dimensional analysis of an axially loaded elastic Rod using 2-node 
Line elements. 

### Constructors
```julia
p41(m, data) # Re-use factored global stiffness matrix
```
### Arguments
```julia
* `m::jFEM`                  : Previously created jFEM model
* `data::Dict{Symbol, Any}`  : Dictionary containing all input data
```

### Required data dictionary keys
```julia
* struc_el::StructuralElement                          : Type of  structural fin_el
* support::Array{Tuple{Int,Array{Int,1}},1}        : Fixed-displacements vector
* loaded_nodes::Array{Tuple{Int,Array{Float64,1}},1} : Node load vector
* properties::Vector{Float64}                          : Material properties
* x_coords::FloatRange{Float64}                        : x-coordinate vector
```

### Optional additional data dictionary keys
```julia
* penalty = 1e20               : Penalty used for fixed degrees of freedoms
* etype::Vector{Int}         : Element material vector if np_types > 1
* eq_nodal_forces_and_moments  : Contribution of distributed loads to loaded_nodes
```

### Return values
```julia
* (jfem, dis_df, fm_df)        : Tuple of jFem, dis_df and fm_df
                                 where:
                                    jfem::jFem    : Computational result type
                                    dis_df        : Displacement data table
                                    fm_df         : Forces and moments data table
```


### Related help
```julia
?StructuralElement             : List of available structural element types
?Rod                           : Help on a Rod structural element
?FiniteElement                 : List finite element types
?Line                          : Help on Line finite element
```
"""
function p41(m::PtFEM.jFEM, data::Dict)
  loads = OffsetArray(zeros(m.neq+1), 0:m.neq)
  if :loaded_nodes in keys(data)
    for i in 1:size(data[:loaded_nodes], 1)
      loads[m.nf[:, data[:loaded_nodes][i][1]]] = data[:loaded_nodes][i][2]
    end
  end

  loads[1:m.neq] = m.cfgsm \ loads[1:m.neq]
  println()

  displacements = zeros(size(m.nf))
  for i in 1:size(displacements, 1)
    for j in 1:size(displacements, 2)
      if m.nf[i, j] > 0
        displacements[i,j] = loads[m.nf[i, j]]
      end
    end
  end
  displacements = displacements'

  # Set lengths of elements
  ell = zeros(length(m.x_coords)-1)
  for i in 1:length(m.x_coords)-1
    ell[i] = m.x_coords[i+1] - m.x_coords[i]
  end

  km = zeros(m.ndof, m.ndof)
  g = zeros(Int, m.ndof)
  eld = zeros(m.ndof)
  actions = zeros(m.nels, m.ndof)
  loads[0] = 0.0
  for i in 1:m.nels
    km = PtFEM.rod_km!(m.km, m.prop[m.etype[i], 1], ell[i])
    g = m.g_g[:, i]
    eld = loads[g]
    actions[i, :] = km * eld
  end
  
  dis_df = DataFrame(
    x_translation = displacements[:, 1],
  )

  if :eq_nodal_forces_and_moments in keys(data)
    fm_df = DataFrame(
    normal_force_1 = actions[:, 1],
    normal_force_2 = actions[:, 2],
    nf_1_uncorrected = actions[:, 1],
    nf_2_uncorrected = actions[:, 2]
    )
  else
    fm_df = DataFrame(
      normal_force_1 = actions[:, 1],
      normal_force_2 = actions[:, 2]
    )
  end
  
  # Correct element forces and moments for equivalent nodal
  # forces and moments introduced for loading between nodes
  if :eq_nodal_forces_and_moments in keys(data)
    eqfm = data[:eq_nodal_forces_and_moments]
    k = data[:struc_el].fin_el.nod * data[:struc_el].fin_el.nodof
    for t in eqfm
      vals = convert(Array, fm_df[t[1], :])
      for i in 1:k
        fm_df[t[1], i] = round.(vals[i] - t[2][i], digits=2)
      end
    end
  end

  fem = PtFEM.jFEM(m.struc_el, m.fin_el, m.ndim, m.nels, m.nst, m.ndof, m.nn, m.nodof,
    m.neq, m.penalty, m.etype, g, m.g_g, m.g_num, m.nf, m.no,
    m.node, m.num, m.sense, actions, m.bee, m.coord, m.gamma, m.dee,
    m.der, m.deriv, displacements, eld, m.fun, m.gc, m.g_coord, m.jac,
    km, m.mm, m.kg, m.cfgsm, loads, m.points, m.prop, m.sigma, m.value,
    m.weights, m.x_coords, m.y_coords, m.z_coords, m.axial)

  (fem, dis_df, fm_df)
end