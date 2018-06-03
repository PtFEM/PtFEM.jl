"""
# p43

Analysis of elastic beams using 2-node Beam structural elements and Line
finite elements. Elastic foundation is optional.

### Constructors
```julia
p43(data)
```
### Arguments
```julia
* `data::Dict{Symbol, Any}` : Dictionary containing all input data
```

### Dictionary keys
```julia
* struc_el::StructuralElement                          : Type of  structural fin_el
* support::Array{Tuple{Int,Array{Int,1}},1}        : Fixed-displacements vector
* loaded_nodes::Array{Tuple{Int,Array{Float64,1}},1} : Node load vector
* properties::Vector{Float64}                          : Material properties
* x_coords::LinSpace{Float64}                          : x coordinate vector
* g_num::Array{Int,2}                                : Element node connections
* fixed_freedoms::Array{Tuple{Vector{Int}}           : Fixed freedoms
```

### Optional additional dictionary keys
```julia
* etype::Vector{Int}                                 : Element material vector
* penalty::Float64                                     : Penalty for fixed freedoms
* eq_nodal_forces_and_moments                          : Equivalent nodal loads
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
?StructuralElement  : Help on structural elements
?Rod                : Help on a Rod structural fin_el
?FiniteElement      : Help on finite element types
```
"""
function p43(data::Dict)
  
  # Parse & check FEdict data
  
  if :struc_el in keys(data)
    struc_el = data[:struc_el]
  else
    println("No fin_el type specified.")
    return
  end
  
  ndim = struc_el.ndim
  nst = struc_el.nst
  
  # Add radial stress
  if ndim == 3 && struc_el.axisymmetric
    nst = 4
  end
  
  fin_el = struc_el.fin_el
  @assert typeof(fin_el) <: FiniteElement
  
  if typeof(fin_el) == Line
    (nels, nn) = PtFEM.mesh_size(fin_el, struc_el.nxe)
  elseif typeof(fin_el) == Triangle || typeof(fin_el) == Quadrilateral
    (nels, nn) = PtFEM.mesh_size(fin_el, struc_el.nxe, struc_el.nye)
  elseif typeof(fin_el) == Hexahedron
    (nels, nn) = PtFEM.mesh_size(fin_el, struc_el.nxe, struc_el.nye, struc_el.nze)
  else
    println("$(typeof(fin_el)) is not a known finite element.")
    return
  end
   
  nodof = ndim                    # Degrees of freedom per node
  ndof = fin_el.nod * nodof      # Degrees of freedom per fin_el
  
  # Update penalty if specified in FEdict
  
  penalty = 1e20
  if :penalty in keys(data)
    penalty = data[:penalty]
  end
  
  # Allocate all arrays
  
  # Start with arrays to be initialized from FEdict
  
  if :properties in keys(data)
    prop = zeros(size(data[:properties], 1), size(data[:properties], 2))
    for i in 1:size(data[:properties], 1)
      prop[i, :] = data[:properties][i, :]
    end
  else
    println("No :properties key found in FEdict")
  end
  
  nf = ones(Int, nodof, nn)
  if :support in keys(data)
    for i in 1:size(data[:support], 1)
      nf[:, data[:support][i][1]] = data[:support][i][2]
    end
  end
  
  x_coords = zeros(nn)
  if :x_coords in keys(data)
    x_coords = data[:x_coords]
  end
  
  y_coords = zeros(nn)
  if :y_coords in keys(data)
    y_coords = data[:y_coords]
  end
  
  z_coords = zeros(nn)
  if :z_coords in keys(data)
    z_coords = data[:z_coords]
  end

  etype = ones(Int, nels)
  if :etype in keys(data)
    etype = data[:etype]
  end
  
  g_num = zeros(Int, fin_el.nod, nels)
  if :g_num in keys(data)
    g_num = data[:g_num]
  end
  
  # All other arrays
  
  points = zeros(struc_el.nip, ndim)
  g = zeros(Int, ndof)
  g_coord = zeros(ndim,nn)
  fun = zeros(fin_el.nod)
  coord = zeros(fin_el.nod, ndim)
  gamma = zeros(nels)
  jac = zeros(ndim, ndim)
  der = zeros(ndim, fin_el.nod)
  deriv = zeros(ndim, fin_el.nod)
  bee = zeros(nst,ndof)
  km = zeros(ndof, ndof)
  mm = zeros(ndof, ndof)
  gm = zeros(ndof, ndof)
  kg = zeros(ndof, ndof)
  eld = zeros(ndof)
  weights = zeros(struc_el.nip)
  g_g = zeros(Int, ndof, nels)
  num = zeros(Int, fin_el.nod)
  actions = zeros(ndof, nels)
  displacements = zeros(size(nf, 1), ndim)
  gc = ones(ndim, ndim)
  dee = zeros(nst,nst)
  sigma = zeros(nst)
  axial = zeros(nels)
  
  # Set global coordinates
  
  formnf!(nodof, nn, nf)
  neq = maximum(nf)
  
  ell = zeros(nels)
  if :x_coords in keys(data)
    for i in 1:length(data[:x_coords])-1
      ell[i] = data[:x_coords][i+1] - data[:x_coords][i]
    end
  end
  
  for i in 1:nels
    num = [i; i+1]
    num_to_g!(num, nf, g)
    g_g[:, i] = g
  end
  
  println("There are $(neq) equations.")
  
  gsm = spzeros(neq, neq)
  for i in 1:nels
    km = beam_km(prop[etype[i], 1], ell[i])
    g = g_g[:, i]
    if size(prop, 2) > 1
      mm = beam_mm(prop[etype[i], 2], ell[i])
    end
    fsparm!(gsm, g, km+mm)
  end
  
  loads = OffsetArray(zeros(neq+1), 0:neq)
  if :loaded_nodes in keys(data)
    for i in 1:size(data[:loaded_nodes], 1)
      loads[nf[:, data[:loaded_nodes][i][1]]] = data[:loaded_nodes][i][2]
    end
  end
  
  fixed_freedoms = 0
  if :fixed_freedoms in keys(data)
    fixed_freedoms = size(data[:fixed_freedoms], 1)
  end
  no = zeros(Int, fixed_freedoms)
  node = zeros(Int, fixed_freedoms)
  sense = zeros(Int, fixed_freedoms)
  value = zeros(Float64, fixed_freedoms)
  if fixed_freedoms > 0
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

  for i in 1:nels
    beam_km(prop[etype[i], 1], ell[i])
    g = g_g[:, i]
    if size(prop, 2) > 1
      beam_mm(prop[etype[i], 2], ell[i])
    end
    eld = zeros(length(g))
    for j in 1:length(g)
      if g[j] != 0
        eld[j] = loads[g[j]]
      end
    end
    actions[:, i] = (km+mm) * eld
  end

  dis_df = DataFrame(
    translations = displacements[1, :],
    rotations = displacements[2, :]
  )

  fm_df = DataFrame(
    xl_Force = actions[1, :],
    xl_Moment = actions[2, :],
    xr_Force = actions[3, :],
    xr_Moment = actions[4, :]
  )
  
  # Correct element forces and moments for equivalent nodal
  # forces and moments introduced for loading between nodes
  if :eq_nodal_forces_and_moments in keys(data)
    eqfm = data[:eq_nodal_forces_and_moments]
    k = data[:struc_el].fin_el.nod * data[:struc_el].fin_el.nodof
    for t in eqfm
      vals = convert(Array, fm_df[t[1], :])
      for i in 1:k
        fm_df[t[1], i] = round.(vals[i] - t[2][i], 2)
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
