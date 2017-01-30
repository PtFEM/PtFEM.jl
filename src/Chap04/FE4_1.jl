using Documenter

"""
# FE4_1

Backbone method for static equilibrium analysis of a rod.

### Constructors
```julia
FE4_1(data::Dict)
```
### Arguments
```julia
* `data` : Dictionary containing all input data
```

### Dictionary keys
```julia
* struc_el::StructuralElement                            : Type of  structural fin_el
* support::Array{Tuple{Int64,Array{Int64,1}},1}        : Fixed-displacements vector
* loaded_nodes::Array{Tuple{Int64,Array{Float64,1}},1} : Node load vector
* properties::Vector{Float64}                          : Material properties
* x_coords::LinSpace{Float64}                          : Xcoordinate vector
```

### Optional dictionary keys
```julia
* etype::Vector{Int64}                                 : Element material vector
```

### Examples
```julia
using CSoM

data = Dict(
  # Rod(nels, np_types, nip, finite_element(nod, nodof))
  :struc_el => Rod(4, 1, 1, Line(2, 1)),
  :properties => [1.0e5;],
  :x_coords => linspace(0, 1, 5),
  :support => [(1, [0])],
  :loaded_nodes => [(1,[-0.625]),(2,[-1.25]),(3,[-1.25]),(4,[-1.25]),(5,[-0.625])]
)

m = FE4_1(data)

println("Displacements:")
m.displacements |> display
println()

println("Actions:")
m.actions |> display
println()

```

### Related help
```julia
?StructuralElement  : Help on structural elements
?Rod                : Help on a Rod structural fin_el
?FiniteElement      : Help on finite element types
```
"""
function FE4_1(data::Dict{Symbol, Any})
  
  # Parse & check FEdict data
  
  if :struc_el in keys(data)
    struc_el = data[:struc_el]
  else
    throw("No fin_el type specified.")
  end
  
  if typeof(struc_el) == CSoM.Rod
    ndim = 1
    nst = struc_el.np_types
  else
    throw("FE4_1 expects a Rod structural fin_el.")
  end
  
  fin_el = struc_el.fin_el
  @assert typeof(fin_el) <: FiniteElement
  
  if typeof(fin_el) == Line
    (nels, nn) = CSoM.mesh_size(fin_el, struc_el.nxe)
  else
    throw("FE4_1 expects a Line finite element.")
  end
   
  nodof = fin_el.nodof         # Degrees of freedom per node
  ndof = fin_el.nod * nodof    # Degrees of freedom per fin_el
  
  # Update penalty if specified in input dict
  
  penalty = 1e20
  if :penalty in keys(data)
    penalty = data[:penalty]
  end
  
  # All dynamic arrays
  
  points = zeros(struc_el.nip, ndim)   # 
  g = zeros(Int64, ndof)                   # Element steering vector
  g_coord = zeros(ndim,nn)                 # 
  fun = zeros(fin_el.nod)                 #
  coord = zeros(fin_el.nod, ndim)         #
  gamma = zeros(nels)                      #
  jac = zeros(ndim, ndim)                  #
  g_num = zeros(Int64, fin_el.nod, nels)  # 
  der = zeros(ndim, fin_el.nod)           #
  deriv = zeros(ndim, fin_el.nod)         #
  bee = zeros(nst,ndof)                    #
  km = zeros(ndof, ndof)                   #
  mm = zeros(ndof, ndof)                   #
  gm = zeros(ndof, ndof)                   #
  kg = zeros(ndof, ndof)                   #
  eld = zeros(ndof)                        #
  weights = zeros(struc_el.nip)        #
  g_g = zeros(Int64, ndof, nels)           #
  num = zeros(Int64, fin_el.nod)          #
  actions = zeros(nels, ndof)              #
  nf = ones(Int64, nodof, nn)              #
  displacements = zeros(size(nf, 1), ndim) #
  gc = ones(ndim, ndim)                    #
  dee = zeros(nst,nst)                     #
  sigma = zeros(nst)                       #
  axial = zeros(nels)                      #
  x_coords = zeros(nn)                     #
  y_coords = zeros(nn)                     # Not used, needed for FEM constructor
  z_coords = zeros(nn)                     #
  etype = ones(Int64, nels)                #

  # Start with arrays to be initialized from input dict
  
  if :properties in keys(data)
    prop = zeros(size(data[:properties], 1), size(data[:properties], 2))
    for i in 1:size(data[:properties], 1)
      prop[i, :] = data[:properties][i, :]
    end
  else
    throw("No :properties key found in input dict to FE4_1.")
  end
  
  # Fill some dynamic arrays from input dict
  
  if :support in keys(data)
    for i in 1:size(data[:support], 1)
      nf[:, data[:support][i][1]] = data[:support][i][2]
    end
  else
    throw("No :support key found in input dict to FE4_1.")
  end

  if :x_coords in keys(data)
    x_coords = data[:x_coords]
  end
  
  if :etype in keys(data)
    etype = data[:etype]
  end
  
  # Done with input dict and allocations
  
  CSoM.formnf!(nodof, nn, nf)
  neq = maximum(nf)
  kdiag = zeros(Int64, neq)
  
  # Set global numbering, coordinates and array sizes
  
  ell = zeros(nels)
  if :x_coords in keys(data)
    for i in 1:length(data[:x_coords])-1
      ell[i] = data[:x_coords][i+1] - data[:x_coords][i]
    end
  end
  
  for i in 1:nels
    num = [i; i+1]
    CSoM.num_to_g!(fin_el.nod, nodof, nn, ndof, num, nf, g)
    g_g[:, i] = g
    CSoM.fkdiag!(ndof, neq, g, kdiag)
  end
  
  for i in 2:neq
    kdiag[i] = kdiag[i] + kdiag[i-1]
  end

  kv = zeros(kdiag[neq])
  gv = zeros(kdiag[neq])
  
  println("There are $(neq) equations and the skyline storage is $(kdiag[neq]).")
    
  loads = zeros(neq+1)
  if :loaded_nodes in keys(data)
    for i in 1:size(data[:loaded_nodes], 1)
      loads[nf[:, data[:loaded_nodes][i][1]]+1] = data[:loaded_nodes][i][2]
    end
  end
  
  for i in 1:nels
    km = CSoM.rod_km!(km, prop[etype[i], 1], ell[i])
    g = g_g[:, i]
    CSoM.fsparv!(kv, km, g, kdiag)
  end

  fixed_freedoms = 0
  if :fixed_freedoms in keys(data)
    fixed_freedoms = size(data[:fixed_freedoms], 1)
  end
  no = zeros(Int64, fixed_freedoms)
  node = zeros(Int64, fixed_freedoms)
  sense = zeros(Int64, fixed_freedoms)
  value = zeros(Float64, fixed_freedoms)
  if :fixed_freedoms in keys(data) && fixed_freedoms > 0
    for i in 1:fixed_freedoms
      node[i] = data[:fixed_freedoms][i][1]
      sense[i] = data[:fixed_freedoms][i][2]
      no[i] = nf[sense[i], node[i]]
      value[i] = data[:fixed_freedoms][i][3]
    end
    kv[kdiag[no]] += penalty
    loads[no+1] = kv[kdiag[no]] .* value
  end

  CSoM.sparin!(kv, kdiag)
  loads[2:end] = CSoM.spabac!(kv, loads[2:end], kdiag)
  println()

  displacements = zeros(size(nf))
  for i in 1:size(displacements, 1)
    for j in 1:size(displacements, 2)
      if nf[i, j] > 0
        displacements[i,j] = loads[nf[i, j]+1]
      end
    end
  end
  displacements = displacements'

  loads[1] = 0.0
  for i in 1:nels
    km = CSoM.rod_km!(km, prop[etype[i], 1], ell[i])
    g = g_g[:, i]
    eld = loads[g+1]
    actions[i, :] = km * eld
  end

  FEM(struc_el, fin_el, ndim, nels, nst, ndof, nn, nodof, neq, penalty,
    etype, g, g_g, g_num, kdiag, nf, no, node, num, sense, actions, 
    bee, coord, gamma, dee, der, deriv, displacements, eld, fun, gc,
    g_coord, jac, km, mm, gm, kv, gv, loads, points, prop, sigma, value,
    weights, x_coords, y_coords, z_coords, axial)
end
