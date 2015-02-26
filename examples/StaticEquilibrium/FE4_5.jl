function FE4_5(data::Dict)
  
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
  if ndim == 3 && typeof(element_type) !== Frame && element_type.axisymmetric
    nst = 4
  end
  
  element = element_type.element
  @assert typeof(element) <: Element
  
  if typeof(element) == Line
    if typeof(element_type) == Frame
      nels = element_type.nels
      nn = element_type.nn
    else  
      (nels, nn) = mesh_size(element, element_type.nxe)
    end
  elseif typeof(element) == Triangle || typeof(element) == Quadrilateral
    (nels, nn) = mesh_size(element, element_type.nxe, element_type.nye)
  elseif typeof(element) == Hexahedron
    (nels, nn) = mesh_size(element, element_type.nxe, element_type.nye, element_type.nze)
  else
    println("$(typeof(element)) is not a known finite element.")
    return
  end
  
  if ndim == 1
    nodof = 2
  else
    nodof = ndim == 2 ? 3 : 6     # Degrees of freedom per node
  end
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
  
  gamma = zeros(nels)
  if :gamma in keys(data)
    gamma = data[:gamma]
  end
  
  
  # All other arrays
  
  points = zeros(element_type.nip, ndim)
  g = zeros(Int64, ndof)
  g_coord = zeros(ndim,nn)
  fun = zeros(element.nod)
  coord = zeros(element.nod, ndim)
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
  
  g_coord[1,:] = data[:x_coords]
  if ndim > 1
    g_coord[2,:] = data[:y_coords]
  end
  if ndim > 2
    g_coord[3,:] = data[:z_coords]
  end
  
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
    num = g_num[:, i]
    num_to_g!(element.nod, nodof, nn, ndof, num, nf, g)
    g_g[:, i] = g
    fkdiag!(ndof, neq, g, kdiag)
  end
  
  for i in 2:neq
    kdiag[i] = kdiag[i] + kdiag[i-1]
  end
  
  kv = zeros(kdiag[neq])
  gv = zeros(kdiag[neq])
    
  println("There are $(neq) equations and the skyline storage is $(kdiag[neq]).\n")
  
  for i in 1:nels
    num = g_num[:, i]
    coord = g_coord[:, num]'
    km = rigid_jointed!(km, prop, gamma, etype, i, coord)
    g = g_g[:, i]
    fsparv!(kv, km, g, kdiag)
  end
  
  limit = 10
  if :limit in keys(data)
    limit = data[:limit]
  end
  
  tol = 1.0e-5
  if :tol in keys(data)
    tol = data[:tol]
  end
  
  incs = 0
  if :incs in keys(data)
    incs = data[:incs]
  else
    println("No increments for loads specified.")
    return
  end
  
  dload = zeros(incs)
  if :dload in keys(data)
    dload = data[:dload]
  end
  
  loads = zeros(neq+1)
  if :loaded_nodes in keys(data)
    for i in 1:size(data[:loaded_nodes], 1)
      loads[nf[:, data[:loaded_nodes][i][1]]+1] = data[:loaded_nodes][i][2]
    end
  end
  
  inode = zeros(size(data[:loaded_nodes], 1))
  ival = zeros(size(data[:loaded_nodes], 1), size(data[:loaded_nodes][1][2], 2))
  if :loaded_nodes in keys(data)
    for i in 1:size(data[:loaded_nodes], 1)
      inode[i] = data[:loaded_nodes][i][1]
      ival[i, :] = data[:loaded_nodes][i][2]
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
  if fixed_freedoms > 0
    for i in 1:fixed_freedoms
      node[i] = data[:fixed_freedoms][i][1]
      sense[i] = data[:fixed_freedoms][i][2]
      no[i] = nf[sense[i], node[i]]
      value[i] = data[:fixed_freedoms][i][3]
    end
    kv[kdiag[no]] += penalty
    loads[no+1] = kv[kdiag[no]] .* value
  end
  
  sparin!(kv, kdiag)
  
  # Special arrays
  eldtot = zeros(neq+1)
  holdr = zeros(ndof, nels)
  react = zeros(ndof)
  action = zeros(ndof)
  
  local converged = true
  
  total_load = 0.0
  for iy in 1:incs
    total_load += dload[iy]
    println("\nLoad step: $(iy), load factor: $total_load")
    bdylds = zeros(neq+1)
    oldlds = zeros(neq+1)
    iters = 0
    while true
      iters += 1
      print(".")
      loads = zeros(neq+1)
      for i in 1:size(inode, 1)
        loads[nf[:, data[:loaded_nodes][i][1]]+1] = dload[iy] * ival[i, :]
      end
      loads += bdylds
      bdylds = zeros(neq+1)
      loads[2:end] = spabac!(kv, loads[2:end], kdiag)
      (oldlds, converged) = checon!(loads, oldlds, tol, converged)
      for iel in 1:nels
        num = g_num[:, iel]
        coord = g_coord[:, num]'
        g = g_g[:, iel]
        eld = loads[g+1]
        km = rigid_jointed!(km, prop, gamma, etype, iel, coord)
        action = km * eld
        react = zeros(ndof)
        if limit !== 1
          hinge!(coord, holdr, action, react, prop, iel, etype, gamma)
          #@show react
          bdylds[g+1] -= react
          bdylds[1] = 0.0
        end
        if iters == limit || converged
          holdr[:, iel] += react[:] + action[:]
        end
      end
      if iters == limit || converged
        break
      end
    end
    eldtot += loads
    println("\n   Node     Displacement(s) and Rotation(s)  (Iterations: $(iters))")
    for i in 1:size(inode, 1)
      println("    $(inode[i])  $(eldtot[nf[:, inode[i]]+1])")
    end
    if iters == limit && limit !== 1
      break
    end
  end
  (inode[size(inode, 1)], eldtot[nf[:, inode[size(inode, 1)]]+1])
end
