function FE4_3(data::Dict)
  
  # Parse & check FEdict data
  
  if :element_type in keys(data)
    element_type = data[:element_type]
  else
    println("No element type specified.")
    return
  end
  
  ndim = element_type.ndim
  nst = element_type.nst
  
  # Add radial stress
  if ndim == 3 && element_type.axisymmetric
    nst = 4
  end
  
  element = element_type.element
  @assert typeof(element) <: Element
  
  if typeof(element) == Line
    (nels, nn) = mesh_size(element, element_type.nxe)
  elseif typeof(element) == Triangle || typeof(element) == Quadrilateral
    (nels, nn) = mesh_size(element, element_type.nxe, element_type.nye)
  elseif typeof(element) == Hexahedron
    (nels, nn) = mesh_size(element, element_type.nxe, element_type.nye, element_type.nze)
  else
    println("$(typeof(element)) is not a known finite element.")
    return
  end
   
  nodof = ndim                    # Degrees of freedom per node
  ndof = element.nod * nodof      # Degrees of freedom per element
  
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
  
  nf = ones(Int64, nodof, nn)
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

  etype = ones(Int64, nels)
  if :etype in keys(data)
    etype = data[:etype]
  end
  
  g_num = zeros(Int64, element.nod, nels)
  if :g_num in keys(data)
    g_num = data[:g_num]
  end
  
  # All other arrays
  
  points = zeros(element_type.nip, ndim)
  g = zeros(Int64, ndof)
  g_coord = zeros(ndim,nn)
  fun = zeros(element.nod)
  coord = zeros(element.nod, ndim)
  gamma = zeros(nels)
  jac = zeros(ndim, ndim)
  der = zeros(ndim, element.nod)
  deriv = zeros(ndim, element.nod)
  bee = zeros(nst,ndof)
  km = zeros(ndof, ndof)
  mm = zeros(ndof, ndof)
  gm = zeros(ndof, ndof)
  kg = zeros(ndof, ndof)
  eld = zeros(ndof)
  weights = zeros(element_type.nip)
  g_g = zeros(Int64, ndof, nels)
  num = zeros(Int64, element.nod)
  actions = zeros(ndof, nels)
  displacements = zeros(size(nf, 1), ndim)
  gc = ones(ndim, ndim)
  dee = zeros(nst,nst)
  sigma = zeros(nst)
  axial = zeros(nels)
  
  # Set global coordinates
  
  formnf!(nodof, nn, nf)
  neq = maximum(nf)
  kdiag = int(zeros(neq))
  
  ell = zeros(nels)
  if :x_coords in keys(data)
    for i in 1:length(data[:x_coords])-1
      ell[i] = data[:x_coords][i+1] - data[:x_coords][i]
    end
  end
  
  for i in 1:nels
    num = [i; i+1]
    num_to_g!(element.nod, nodof, nn, ndof, num, nf, g)
    g_g[:, i] = g
    fkdiag!(ndof, neq, g, kdiag)
  end
  
  for i in 2:neq
    kdiag[i] = kdiag[i] + kdiag[i-1]
  end

  kv = zeros(kdiag[neq])
  gv = zeros(kdiag[neq])
    
  println("There are $(neq) equations and the skyline storage is $(kdiag[neq]).")
  
  for i in 1:nels
    km = beam_km!(km, prop[etype[i], 1], ell[i])
    g = g_g[:, i]
    if size(prop, 2) > 1
      mm = beam_mm!(mm, prop[etype[i], 2], ell[i])
    end
    fsparv!(kv, km+mm, g, kdiag)
  end
  @show kv
  
  loads = zeros(neq+1)
  if :loaded_nodes in keys(data)
    for i in 1:size(data[:loaded_nodes], 1)
      loads[nf[:, data[:loaded_nodes][i][1]]+1] = data[:loaded_nodes][i][2]
    end
  end
  
  fixed_freedoms = 0
  if :fixed_freedoms in keys(data)
    fixed_freedoms = size(data[:fixed_freedoms], 1)
  end
  no = zeros(Int64, fixed_freedoms)
  node = zeros(Int64, fixed_freedoms)
  sense = zeros(Int64, fixed_freedoms)
  value = zeros(Float64, fixed_freedoms)
  @show fixed_freedoms
  @show kdiag
  @show nf
  if fixed_freedoms > 0
    for i in 1:fixed_freedoms
      node[i] = data[:fixed_freedoms][i][1]
      sense[i] = data[:fixed_freedoms][i][2]
      no[i] = nf[sense[i], node[i]]
      value[i] = data[:fixed_freedoms][i][3]
    end
    @show node
    @show sense
    @show no
    @show value
    kv[kdiag[no]] += penalty
    loads[no+1] = kv[kdiag[no]] .* value
  end
  
  @show kv
  @show loads
  println()
  
  sparin!(kv, kdiag)
  @show kv
  println()
  
  loads[2:end] = spabac!(kv, loads[2:end], kdiag)
  @show kv
  @show loads


  displacements = zeros(size(nf))
  for i in 1:size(displacements, 1)
    for j in 1:size(displacements, 2)
      if nf[i, j] > 0
        displacements[i,j] = loads[nf[i, j]+1]
      end
    end
  end

  for i in 1:nels
    beam_km!(km, prop[etype[i], 1], ell[i])
    g = g_g[:, i]
    if size(prop, 2) > 1
      beam_mm!(mm, prop[etype[i], 2], ell[i])
    end
    eld = zeros(length(g))
    for j in 1:length(g)
      if g[j] != 0
        eld[j] = loads[g[j]+1]
      end
    end
    actions[:, i] = (km+mm) * eld
  end

  #=
  @show typeof(actions)
  @show typeof(bee)
  @show typeof(coord)
  @show typeof(gamma)
  @show typeof(dee)
  @show typeof(der)
  @show typeof(deriv)
  @show typeof(displacements)
  @show typeof(eld)
  @show typeof(fun)
  @show typeof(gc)
  @show typeof(g_coord)
  @show typeof(jac)
  @show typeof(km)
  @show typeof(mm)
  @show typeof(kg)
  @show typeof(kv)
  @show typeof(gv)
  @show typeof(loads)
  @show typeof(points)
  @show typeof(prop)
  @show typeof(sigma)
  @show typeof(value)
  @show typeof(weights)
  @show typeof(x_coords)
  @show typeof(y_coords)
  =#
  
  FEM(element_type, element, ndim, nels, nst, ndof, nn, nodof, neq, penalty,
    etype, g, g_g, g_num, kdiag, nf, no, node, num, sense, actions, 
    bee, coord, gamma, dee, der, deriv, displacements, eld, fun, gc,
    g_coord, jac, km, mm, gm, kv, gv, loads, points, prop, sigma, value,
    weights, x_coords, y_coords, z_coords, axial)
end
