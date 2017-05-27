"""
# Method p51_skyline

Plane or axisymmetric strain analysis of an elastic solid (Plane
structural element) using 3-, 6-, 10- or 15-node right-angled triangles
(Triangle finite elements) or 4-, 8- or 9-node rectangular quadrilaterals
(Quadrilateral finite elements). Mesh numbered in x(r)- or y(z)- direction.

### Constructors
```julia
p51(data)
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
* etype::Vector{Int}           : Element material vector if np_types > 1
```

### Return values
```julia
* fem                          : Fem object
```

### Related help
```julia
?StructuralElement             : List of available structural element types
?Plane                         : Help on a Plane structural element
?FiniteElement                 : List finite element types
?Quadrilateral                 : Help on Quadrilateral finite element
```
"""
function p51_skyline(data::Dict{Symbol, Any})
  
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
  struc_el.axisymmetric && (nst = 4)
  
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
  kdiag = zeros(Int, neq)
  
  # Find global array sizes
  
  for iel in 1:nels
    geom_rect!(fin_el, iel, x_coords, y_coords, coord, num, struc_el.direction)
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
  gv = zeros(kdiag[neq])

  println("There are $(neq) equations and the skyline storage is $(kdiag[neq]).")
  
  sample!(fin_el, points, weights)
  
  for iel in 1:nels
    deemat!(dee, prop[etype[iel], 1], prop[etype[iel], 2])
    num = g_num[:, iel]
    coord = g_coord[:, num]'              # Transpose
    g = g_g[:, iel]
    km = zeros(ndof, ndof)
    for i in 1:struc_el.nip
      shape_fun!(fun, points, i)
      shape_der!(der, points, i)
      jac = der*coord
      detm = det(jac)
      jac = inv(jac)
      deriv = jac*der
      beemat!(bee, deriv)
      if struc_el.axisymmetric
        gc = fun'*coord
        bee[4, 1:2:(ndof-1)] = fun[:]/gc[1]
      end
      km += (bee')*dee*bee*detm*weights[i]*gc[1]
    end
    fsparv(kv, km, g, kdiag)
  end
  
  loads = zeros(neq + 1)
  if :loaded_nodes in keys(data)
    for i in 1:size(data[:loaded_nodes], 1)
      loads[nf[:, data[:loaded_nodes][i][1]]+1] = data[:loaded_nodes][i][2]
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
    end
    kv[kdiag[no]] = kv[kdiag[no]] + penalty
    loads[no+1] = kv[kdiag[no]] .* value
  end
  
  sparin!(kv, kdiag)
  loads[2:end] = spabac!(kv, loads[2:end], kdiag)
  loads[1] = 0.0
  nf1 = deepcopy(nf) + 1
  
  if struc_el.axisymmetric
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
  
  struc_el.nip = 1
  points = zeros(struc_el.nip, ndim)
  weights = zeros(struc_el.nip)
  sample!(fin_el, points, weights)
  println("\nThe integration point (nip = $(struc_el.nip)) stresses are:")
  if struc_el.axisymmetric
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
    for i in 1:struc_el.nip
      shape_fun!(fun, points, i)
      shape_der!(der, points, i)
      gc = fun'*coord
      jac = inv(der*coord)
      deriv = jac*der
      beemat!(bee, deriv)
      if struc_el.axisymmetric
        gc = fun'*coord
        bee[4, 1:2:(ndof-1)] = fun[:]/gc[1]
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
  
  FEM(struc_el, fin_el, ndim, nels, nst, ndof, nn, nodof, neq, penalty,
    etype, g, g_g, g_num, kdiag, nf, no, node, num, sense, actions, 
    bee, coord, gamma, dee, der, deriv, displacements, eld, fun, gc,
    g_coord, jac, km, mm, gm, kv, gv, loads, points, prop, sigma, value,
    weights, x_coords, y_coords, z_coords, axial)
  
end

