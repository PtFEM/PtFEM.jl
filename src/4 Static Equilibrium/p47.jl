"""
# Method p47 

Analysis of plates (Plane structural element) using 4-node Quadrilateral
finite elements. Homogeneous material with identical elements. Mesh
numbered in x or y direction.

### Constructors
```julia
p47(data)
```
### Arguments
```julia
* `data::Dict{Symbol, Any}` : Dictionary containing all input data
```

### Required data dictionary keys
```julia
* struc_el::StructuralElement                          : Structural element
* support::Array{Tuple{Int,Array{Int,1}},1}        : Fixed-displacements vector
* loaded_nodes::Array{Tuple{Int,Array{Float64,1}},1} : Node load vector
* properties::Vector{Float64}                          : Material properties
* x_coords::FloatRange{Floalt64}                       : x-coordinate vector
* y_coords::FloatRange{Floalt64}                       : y-coordinate vector
* thickness:: Float64                                  : Thickness of plate
```

### Optional additional data dictionary keys
```julia
* penalty = 1e20               : Penalty used for fixed degrees of freedoms
* etype::Vector{Int}         : Element material vector if np_types > 1
```

### Return values
```julia
* (fm_df, sigma_df)            : Tuple of jFem, dis_df and fm_df
                                  where:
                                    fm_df         : Forces and moments data table
                                    sigma_df      : Stresses data table
```

### Related help
```julia
?StructuralElement             : List of available structural element types
?Plane                         : Help on a Plane structural element
?FiniteElement                 : List finite element types
?Quadrilateral                 : Help on Quadrilateral finite element
```
"""
function p47(data::Dict{Symbol, Any})
  
  # Setup basic dimensions of arrays
  
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
    (nels, nn) = mesh_size(fin_el, struc_el.nxe)
  elseif typeof(fin_el) == Triangle || typeof(fin_el) == Quadrilateral
    (nels, nn) = mesh_size(fin_el, struc_el.nxe, struc_el.nye)
  elseif typeof(fin_el) == Hexahedron
    (nels, nn) = mesh_size(fin_el, struc_el.nxe, struc_el.nye, struc_el.nze)
  else
    println("$(typeof(fin_el)) is not a known finite element.")
    return
  end
     
  nodof = fin_el.nodof           # Degrees of freedom per node
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
  
  # All other arrays
  
  points = zeros(struc_el.nip, ndim)
  g = zeros(Int, ndof)
  g_coord = zeros(ndim,nn)
  fun = zeros(fin_el.nod)
  coord = zeros(fin_el.nod, ndim)
  gamma = zeros(nels)
  jac = zeros(ndim, ndim)
  g_num = zeros(Int, fin_el.nod, nels)
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
  gc = ones(ndim)
  dee = zeros(nst,nst)
  sigma = zeros(nst)
  axial = zeros(nels)
  
  formnf!(nodof, nn, nf)
  neq = maximum(nf)
  
  # Find global array sizes
  
  for iel in 1:nels
    geom_rect!(fin_el, iel, x_coords, y_coords, coord, num, struc_el.direction)
    num_to_g!(num, nf, g)
    g_num[:, iel] = num
    g_coord[:, num] = coord'
    g_g[:, iel] = g
  end
  println()

  println("There are $(neq) equations.")
  
  sample!(fin_el, points, weights)
  
  d2x = zeros(neq)
  d2y = zeros(neq)
  d2xy = zeros(neq)
  dtd = zeros(ndof, ndof)
  @assert :thickness in keys(data)
  aa = x_coords[2]-x_coords[1]
  bb = y_coords[2]-y_coords[1]
  
  gsm = spzeros(neq, neq)
  for iel in 1:nels
    e = prop[etype[iel], 1]
    v = prop[etype[iel], 2]
    d = e * data[:thickness]^3 / (12.0 * (1.0 - v * v))
    g = g_g[:, iel]
    fill!(km, 0)
    for i in 1:struc_el.nip
      fmplat!(d2x, d2y, d2xy, points, aa, bb, i)
      for k in 1:ndof
        tmp = (d2x[k]*d2x[:]/aa^4 + d2y[k]*d2y[:]/bb^4 +
        (v*d2x[k]*d2y[:]+v*d2x[:]*d2y[k] + 2.0*(1.0-v)*d2xy[k]*d2xy[:])/(aa^2*bb^2))
        dtd[k, :] = 4.0aa*bb*d*weights[i]*tmp 
        dtd[:, k] = dtd[k, :]
      end
      km = km + dtd
    end
    fsparm!(gsm, g, km)
  end
  println()
  
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
  if :fixed_freedoms in keys(data) && fixed_freedoms > 0
    for i in 1:fixed_freedoms
      no[i] = nf[data[:fixed_freedoms][i][2], data[:fixed_freedoms][i][1]]
      value[i] = data[:fixed_freedoms][i][3]
      gsm[no[i], no[i]] += penalty
      loads[no[i]] = gsm[no[i], no[i]] * value
    end
  end
  
  cfgsm = cholesky(gsm)
  loads[1:neq] = cfgsm \ loads[1:neq]
  
  disp = Float64[]
  rotx = Float64[]
  roty = Float64[]
  twistxy = Float64[]
  for i in 1:nn
    append!(disp, loads[nf[1,i]])
    append!(rotx, loads[nf[2,i]])
    append!(roty, loads[nf[3,i]])
    append!(twistxy, loads[nf[4,i]])
  end
  dis_df = DataFrame(
    disp = disp,
    rotx = rotx,
    roty = roty,
    twistxy = twistxy
  )

  struc_el.nip = 1
  points = zeros(struc_el.nip, ndim)
  weights = zeros(struc_el.nip)
  
  sample!(fin_el, points, weights)
  
  sigx = Float64[]
  sigy = Float64[]
  tauxy = Float64[]
  
  bm = Vector{Float64}
  for iel in 1:nels
    e = prop[etype[iel], 1]
    v = prop[etype[iel], 2]
    d = e * data[:thickness]^3 / (12.0 * (1.0 - v * v))
    g = g_g[:, iel]
    for i in 1:struc_el.nip
      fmplat!(d2x, d2y, d2xy, points, aa, bb, i)
      bm = zeros(3)
      for k in 1:ndof
        bm[1] += 4.0*d*(d2x[k]/aa/aa + v*d2y[k]/bb/bb)*loads[g[k]]
        bm[2] += 4.0*d*(v*d2x[k]/aa/aa + d2y[k]/bb/bb)*loads[g[k]]
        bm[3] += 4.0*d*(1.0-v)*(d2xy[k]/aa/bb*loads[g[k]])
      end
      append!(sigx, bm[1])
      append!(sigy, bm[2])
      append!(tauxy, bm[3])
    end
  end
  
  sigma_df = DataFrame(
    sigx = sigx,
    sigy = sigy,
    tauxy = tauxy
  )
  
  (dis_df, sigma_df)
end

