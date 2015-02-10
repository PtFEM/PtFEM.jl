function FEbeam(data::Dict)
  
  ndim = 3
  nst = 1
  ndim == 2 ? nodof = 3 : nodof = 6           # Degrees of freedom per node
  nprops = 4
  penalty = 1e20
  
  if :element_type in keys(data)
    element_type = data[:element_type]
  else
    println("No element type specified.")
    return
  end
  
  element = element_type.element
  
  if :properties in keys(data)
    prop = zeros(size(data[:properties], 2), size(data[:properties], 1))
    for i in 1:size(data[:properties], 1)
      prop[:, i] = data[:properties][i, :]
    end
  end
  
  (nels, nn) = mesh_size(element, element.nod, element_type.nxe)
  
  ndof = element.nod * nodof                  # Degrees of freedom per element
  
  if typeof(element) == Axisymmetric
    nst = 4
  end
  
  # Allocate all arrays
  nf = ones(Int64, nodof, nn)
  if :support in keys(data)
    for i in 1:size(data[:support], 1)
      nf[:, data[:support][i][1]] = data[:support][i][2]
    end
  end
  
  points = zeros(element_type.nip, ndim)
  g = zeros(Int64, ndof)
  g_coord = zeros(ndim,nn)
  x_coords = zeros(nn)
  y_coords = zeros(nn)
  z_coords = zeros(nn)
  fun = zeros(element.nod)
  coord = zeros(element.nod, ndim)
  gamma = zeros(nels)
  jac = zeros(ndim, ndim)
  g_num = zeros(Int64, element.nod, nels)
  der = zeros(ndim, element.nod)
  deriv = zeros(ndim, element.nod)
  bee = zeros(nst,ndof)
  km = zeros(ndof, ndof)
  eld = zeros(ndof)
  weights = zeros(element_type.nip)
  g_g = zeros(Int64, ndof, nels)
  num = zeros(Int64, element.nod)
  if :x_coords in keys(data)
    x_coords = data[:x_coords]
  end
  if :y_coords in keys(data)
    y_coords = data[:y_coords]
  else
    y_coords = zeros(length(x_coords))
  end
  if :z_coords in keys(data)
    z_coords = data[:z_coords]
  else
    z_coords = zeros(length(z_coords))
  end
  actions = zeros(ndof, nels)
  displacements = zeros(size(nf, 1), ndim)
  
  etype = ones(Int64, nels)
  if :etype in keys(data)
    etype = data[:etype]
  end
  
  gc = ones(ndim, ndim)
  dee = zeros(nst,nst)
  sigma = zeros(nst)
  
  formnf!(nodof, nn, nf)
  neq = maximum(nf)
  kdiag = int(zeros(neq))
  loads = zeros(length(nf))
  
  g_num[1, :] = int(linspace(1, nels, nels))'
  g_num[2, :] = int(linspace(2, nels+1, nels))'

  if :x_coords in keys(data)
    g_coord[1,:] = data[:x_coords]
  end
    
  for i in 1:nels
    num = g_num[:, i]
    num_to_g!(element.nod, nodof, nn, ndof, num, nf, g)
    g_g[:, i] = g
    fkdiag!(ndof, neq, g, kdiag)
  end
  
  for i in 2:neq
    kdiag[i] = kdiag[i] + kdiag[i-1]
  end

  kv = zeros(kdiag[neq])
  
  println("There are $(neq) equations and the skyline storage is $(kdiag[neq]).")
  
  loads = zeros(neq)
  if :loaded_nodes in keys(data)
    for i in 1:size(data[:loaded_nodes], 1)
      loads[nf[:, data[:loaded_nodes][i][1]]] = data[:loaded_nodes][i][2]
    end
  end
  
  fixed_freedoms = 0
  if :fixed_freedoms in keys(data)
    fixed_freedoms = size(data[:fixed_freedoms], 1)
  end
  no = zeros(Int64, fixed_freedoms)
  node = zeros(Int64, fixed_freedoms)
  sense = zeros(Int64, fixed_freedoms)
  value = zeros(Int64, fixed_freedoms)
  if :fixed_freedoms in keys(data) && fixed_freedoms > 0
    for i in 1:fixed_freedoms
      no[i] = nf[data[:fixed_freedoms][i][2], data[:fixed_freedoms][i][1]]
      value[i] = data[:fixed_freedoms][i][3]
    end
    kv[kdiag[no]] = kv[kdiag[no]] + penalty
    loads[no] = kv[kdiag[no]] * value
  end
  
  #=
  println(g_coord)
  println()
  println(kdiag)
  println()
  println(loads)
  println()
  println(fixed_freedoms)
  println()
  println(prop)
  println()
  println(etype)
  println()
  =#
  
  for i in 1:nels
    num = g_num[:, i]
    coord = g_coord[:, num]'              #'
    rigid_jointed!(km, prop, gamma, etype, i, coord)
    g = g_g[:, i]
    fsparv!(kv, km, g, kdiag)
  end
  
  sparin!(kv, kdiag)
  spabac!(kv, loads, kdiag)
  #nf1 = deepcopy(nf) + 1

  displacements = zeros(size(nf))
  for i in 1:size(displacements, 1)
    for j in 1:size(displacements, 2)
      if nf[i, j] > 0
        displacements[i,j] = loads[nf[i, j]]
      end
    end
  end

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
  @show typeof(kv)
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
    g_coord, jac, km, kv, loads, points, prop, sigma, value, weights,
    x_coords, y_coords)
end
