function p42(data::Dict)
  
  # Parse & check FEdict data
  
  if :struc_el in keys(data)
    struc_el = data[:struc_el]
  else
    println("No fin_el type specified.")
    return
  end
  
  nels = struc_el.nels
  nn = struc_el.nn
  ndim = struc_el.ndim
  nip = struc_el.nip
  nst = struc_el.nst
  
  fin_el = struc_el.fin_el
  @assert typeof(fin_el) <: FiniteElement
  
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
  else
    y_coords = zeros(length(x_coords))
  end
  
  z_coords = zeros(nn)
  if :z_coords in keys(data)
    z_coords = data[:z_coords]
  else
    z_coords = zeros(length(z_coords))
  end

  etype = ones(Int64, nels)
  if :etype in keys(data)
    etype = data[:etype]
  end
  
  g_num = zeros(Int64, fin_el.nod, nels)
  if :g_num in keys(data)
    g_num = data[:g_num]
  end
  
  # All other arrays
  
  points = zeros(struc_el.nip, ndim)
  g = zeros(Int64, ndof)
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
  g_g = zeros(Int64, ndof, nels)
  num = zeros(Int64, fin_el.nod)
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
  
  for i in 1:nels
    num = g_num[:, i]
    num_to_g!(fin_el.nod, nodof, nn, ndof, num, nf, g)
    g_g[:, i] = g
  end
  
  println("There are $(neq) equations.")
  
  loads = zeros(neq+1)
  if :loaded_nodes in keys(data)
    for i in 1:size(data[:loaded_nodes], 1)
      loads[nf[:, data[:loaded_nodes][i][1]]+1] = data[:loaded_nodes][i][2]
    end
  end
  
  gsm = spzeros(neq, neq)
  for i in 1:nels
    num = g_num[:, i]
    coord = g_coord[:, num]'              #'
    km = pin_jointed!(km, prop[etype[i], 1], coord)
    g = g_g[:, i]
    fsparm!(gsm, g, km)
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
      gsm[no[i], no[i]] += penalty
      loads[no[i] + 1] = gsm[no[i], no[i]] .* value[i]
    end
  end
  
  cfgsm = cholfact(gsm)
  loads[2:end] = cfgsm \ loads[2:end]
  println()

  displacements = zeros(size(nf))
  for i in 1:size(displacements, 1)
    for j in 1:size(displacements, 2)
      if nf[i, j] > 0
        displacements[i,j] = loads[nf[i, j]+1]
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
        eld[j] = loads[g[j]+1]
      end
    end
    km = pin_jointed!(km, prop[etype[i], 1], coord)
    actions[:, i] = km * eld
    axial[i] = global_to_axial(actions[:, i], coord)
  end

  if ndim == 2
    dis_dt = DataTable(
      x_translation = displacements[1, :],
      y_translation = displacements[2, :],
    )
    fm_dt = DataTable(
      x_force_1 = actions[1, :],
      y_force_1 = actions[2, :],
      x_force_2 = actions[3, :],
      y_force_2 = actions[4, :],
      axial_force = axial
    )
  elseif ndim == 3
    dis_dt = DataTable(
      x_translation = displacements[1, :],
      y_translation = displacements[2, :],
      z_translation = displacements[3, :],
    )
    fm_dt = DataTable(
      x_force_1 = actions[1, :],
      y_force_1 = actions[2, :],
      z_force_1 = actions[3, :],
      x_force_2 = actions[4, :],
      y_force_2 = actions[5, :],
      z_force_2 = actions[6, :],
      axial_force = axial
    )
    
  end
  # Correct element forces and moments for equivalent nodal
  # forces and moments introduced for loading between nodes
  if :eq_nodal_forces_and_moments in keys(data)
    eqfm = data[:eq_nodal_forces_and_moments]
    k = data[:struc_el].fin_el.nod * data[:struc_el].fin_el.nodof
    for t in eqfm
      vals = convert(Array, fm_dt[t[1], :])
      for i in 1:k
        fm_dt[t[1], i] = round(vals[i] - t[2][i], 2)
      end
    end
  end

  fem = PtFEM.jFEM(struc_el, fin_el, ndim, nels, nst, ndof, nn, nodof,
    neq, penalty, etype, g, g_g, g_num, nf, no,
    node, num, sense, actions, bee, coord, gamma, dee,
    der, deriv, displacements, eld, fun, gc, g_coord, jac,
    km, mm, kg, cfgsm, loads, points, prop, sigma, value,
    weights, x_coords, y_coords, z_coords, axial)

  (fem, dis_dt, fm_dt)
end
