function p56(data::Dict, profiling::Bool=false)
  
  # Setup basic dimensions of arrays
  
  # Parse & check FEdict data
  
  if :struc_el in keys(data)
    #struc_el = deepcopy(data[:struc_el])
    struc_el = data[:struc_el]
    @assert typeof(struc_el) <: StructuralElement
  else
    println("No fin_el type specified.")
    return
  end
  
  ndim = copy(struc_el.ndim)
  nst = copy(struc_el.nst)
  
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
     
  nodof = copy(fin_el.nodof)           # Degrees of freedom per node
  ndof = fin_el.nod * nodof      # Degrees of freedom per fin_el
  
  # Update penalty if specified in FEdict
  
  penalty = 1e20
  if :penalty in keys(data)
    penalty = copy(data[:penalty])
  end
  
  # Allocate all arrays
  
  # Start with arrays to be initialized from FEdict
  
  if :properties in keys(data)
    prop = zeros(size(data[:properties], 1), size(data[:properties], 2))
    for i in 1:size(data[:properties], 1)
      prop[i, :] = deepcopy(data[:properties][i, :])
    end
  else
    println("No :properties key found in FEdict")
  end
    
  nf = ones(Int, nodof, nn)
  if :support in keys(data)
    for i in 1:size(data[:support], 1)
      nf[:, data[:support][i][1]] = deepcopy(data[:support][i][2])
    end
  end
  
  x_coords = zeros(nn)
  if :x_coords in keys(data)
    x_coords = deepcopy(data[:x_coords])
  end
  
  y_coords = zeros(nn)
  if :y_coords in keys(data)
    y_coords = deepcopy(data[:y_coords])
  end
  
  z_coords = zeros(nn)
  if :z_coords in keys(data)
    z_coords = deepcopy(data[:z_coords])
  end

  etype = ones(Int, nels)
  if :etype in keys(data)
    etype = deepcopy(data[:etype])
  end
  
  g_coord = zeros(ndim,nn)
  if :g_coord in keys(data)
    g_coord = deepcopy(data[:g_coord]')
  end
  
  g_num = zeros(Int, fin_el.nod, nels)
  if :g_num in keys(data)
    g_num = reshape(deepcopy(data[:g_num]'), fin_el.nod, nels)
  end
  
  etype = ones(Int, nels)
  if :etype in keys(data)
    etype = deepcopy(data[:etype])
  end
  
  # All other arrays
  
  points = zeros(struc_el.nip, ndim)
  g = zeros(Int, ndof)
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
  gc = ones(ndim)
  dee = zeros(nst,nst)
  sigma = zeros(nst)
  axial = zeros(nels)
  
  formnf!(nodof, nn, nf)
  neq = maximum(nf)
  println("There are $(neq) equations.\n")
  
  # PCG & program specific variables
  storkm = zeros(ndof, ndof, nels)
  cg_converged = false
  solid = false
  
  if :cg_tol in keys(data)
    cg_tol = copy(data[:cg_tol])
  else
    cg_tol = 1.0e-5
  end
  
  if :cg_limit in keys(data)
    cg_limit = copy(data[:cg_limit])
  else
    cg_limit = 200
  end
  
  sample!(fin_el, points, weights)

  loads = OffsetArray(zeros(neq + 1), 0:neq)
  diag_precon = OffsetArray(zeros(neq + 1), 0:neq)
  p = OffsetArray(zeros(neq + 1), 0:neq)
  store = OffsetArray(zeros(neq + 1), 0:neq)
  d = OffsetArray(zeros(neq + 1), 0:neq)
  
  for iel in 1:nels
    hexahedron_xz!(iel, x_coords, y_coords, z_coords, coord, num)
    num_to_g!(num, nf, g)
    g_num[:, iel] = num
    g_coord[:, num] = coord'
    g_g[:, iel] = g
    deemat!(dee, prop[etype[iel], 1], prop[etype[iel], 2])
    num = g_num[:, iel]
    g = g_g[:, iel]
    coord = g_coord[:, num]'              # Transpose
    km = zeros(ndof, ndof)
    for i in 1:struc_el.nip
      shape_der!(der, points, i)
      jac = der*coord
      detm = det(jac)
      jac = inv(jac)
      deriv = jac*der
      beemat!(bee, deriv)
      storkm[:,:,iel] += (bee')*dee*bee*detm*weights[i]
    end
    for k in 1:ndof
      diag_precon[g[k]] += storkm[k,k,iel]
    end
  end
  
  if :loaded_nodes in keys(data)
    for i in 1:size(data[:loaded_nodes], 1)
      loads[nf[:, data[:loaded_nodes][i][1]]] = deepcopy(data[:loaded_nodes][i][2])
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
      no[i] = nf[deepcopy(data[:fixed_freedoms][i][2]), deepcopy(data[:fixed_freedoms][i][1])]
      value[i] = deepcopy(data[:fixed_freedoms][i][3])
      diag_precon[no[i]] += penalty
      loads[no[i]] = diag_precon[no[i]] * value[i]
      store[no[i]] = diag_precon[no[i]]
    end
  end

  #mesh_ensi(...)
  
  diag_precon[1:neq] = 1.0 ./ diag_precon[1:neq]
  diag_precon[0] = 0.0
  
  d = diag_precon .* loads
  p = deepcopy(d)
  x = OffsetArray(zeros(neq + 1), 0:neq)
  xnew = OffsetArray(zeros(neq + 1), 0:neq)
  
  profiling && println("Start of CG loop.")
  cg_iters = 0
  while true
    cg_iters += 1
    !profiling && print(".")
    u = OffsetArray(zeros(neq + 1), 0:neq)
    for iel in 1:nels
      g[:] = g_g[:, iel]
      #km[:,:] = storkm[:, :, iel]
      u[g] += storkm[:, :, iel]*p[g]
    end
    if fixed_freedoms !== 0
      for i in 1:fixed_freedoms
        u[no[i]] = p[no[i]]*store[no[i]]
      end
    end
    up = dot(loads, d)
    alpha = up/dot(p, u)
    xnew = x + p*alpha
    loads .-= u*alpha
    d = diag_precon .* loads
    beta = dot(loads, d)/up
    p = d + p*beta
    cg_converged = checon(xnew, x, cg_tol)
    @show [cg_iters beta] 
    if cg_converged || cg_iters >= cg_limit
      break
    end
  end
  println()
  
  loads = xnew
  loads[0] = 0.0
  
  println("Number of cg iterations to convergence was $(cg_iters).")
  displacements = zeros(size(nf))
  for i in 1:size(displacements, 1)
    for j in 1:size(displacements, 2)
      if nf[i, j] > 0
        displacements[i,j] = loads[nf[i, j]]
      end
    end
  end
  displacements = displacements'
  
  dis_df = DataFrame(
    x_disp = displacements[:, 1],
    y_disp = displacements[:, 2],
    z_disp = displacements[:, 3]
  )
  #dismsh_ensi()
  
  struc_el.nip = 1
  points = zeros(struc_el.nip, ndim)
  weights = zeros(struc_el.nip)
  
  sample!(fin_el, points, weights)
  
  gc1 = Vector{Float64}()
  gc2 = Vector{Float64}()
  gc3 = Vector{Float64}()
  s1 = Vector{Float64}()
  s2 = Vector{Float64}()
  s3 = Vector{Float64}()  
  s4 = Vector{Float64}()
  s5 = Vector{Float64}()
  s6 = Vector{Float64}()  
  
  for iel in 1:nels
    deemat!(dee, prop[etype[iel], 1], prop[etype[iel], 2])
    num = g_num[:, iel]
    coord = g_coord[:, num]'
    g = g_g[:, iel]
    eld = loads[g]
    for i in 1:struc_el.nip
      shape_fun!(fun, points, i)
      shape_der!(der, points, i)
      gc = fun'*coord
      jac = inv(der * coord)
      deriv = jac * der
      beemat!(bee, deriv)
      sigma = dee*(bee*eld)
      gc1 = append!(gc1, gc[1])
      gc2 = append!(gc2, gc[2])
      gc3 = append!(gc3, gc[3])
      s1 = append!(s1, sigma[1])
      s2 = append!(s2, sigma[2])
      s3 = append!(s3, sigma[3])
      s4 = append!(s4, sigma[4])
      s5 = append!(s5, sigma[5])
      s6 = append!(s6, sigma[6])
    end
  end
  
  sigma_df = DataFrame(
    x_coord = gc1,
    y_coord = gc2,
    z_coord = gc3,
    sig_x = s1,
    sig_y = s2,
    sig_z = s3,
    tau_xy = s4,
    tau_yz = s5,
    tau_zx = s6
  )
  
  (dis_df, sigma_df)
end

