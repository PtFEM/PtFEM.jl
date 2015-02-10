function FEmodel(data::Dict)
  
  ndim = 2
  nst = 3
  nodof = 2
  nprops = 2
  penalty = 1e20
  
  if :element_type in keys(data)
    element_type = data[:element_type]
  else
    println("No element type specified.")
    return
  end
  
  element = element_type.element
  
  if :properties in keys(data)
    prop = data[:properties]
  end
    
  (nels, nn) = mesh_size(element, element.nod, element_type.nxe, element_type.nye)
  
  ndof = element.nod * nodof           # Degrees of freedom per element

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
  x_coords = data[:x_coords]
  y_coords = data[:y_coords]
  actions = zeros(ndof, nels)
  displacements = zeros(size(nf, 1), ndim)
  
  etype = ones(Int64, nels)
  if :etype in keys(data)
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
    geom_rect!(element, iel, x_coords, y_coords, coord, num, element_type.direction)
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
    for i in 1:element_type.nip
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
  value = zeros(Int64, fixed_freedoms)
  if :fixed_freedoms in keys(data) && fixed_freedoms > 0
    for i in 1:fixed_freedoms
      no[i] = nf[data[:fixed_freedoms][i][2], data[:fixed_freedoms][i][1]]
      value[i] = data[:fixed_freedoms][i][3]
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
  
  points = zeros(element_type.nip, ndim)
  weights = zeros(element_type.nip)
  sample!(element, points, weights)
  println("\nThe integration point (nip = $(element_type.nip)) stresses are:")
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
    for i in 1:element_type.nip
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

