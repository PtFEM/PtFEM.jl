function FE4_7(data::Dict)
  
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
  
  # All other arrays
  
  points = zeros(struc_el.nip, ndim)
  g = zeros(Int64, ndof)
  g_coord = zeros(ndim,nn)
  fun = zeros(fin_el.nod)
  coord = zeros(fin_el.nod, ndim)
  gamma = zeros(nels)
  jac = zeros(ndim, ndim)
  g_num = zeros(Int64, fin_el.nod, nels)
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
  gc = ones(ndim)
  dee = zeros(nst,nst)
  sigma = zeros(nst)
  axial = zeros(nels)
  
  formnf!(nodof, nn, nf)
  neq = maximum(nf)
  kdiag = zeros(Int64, neq)
  
  # Find global array sizes
  
  for iel in 1:nels
    geom_rect!(fin_el, iel, x_coords, y_coords, coord, num, struc_el.direction)
    num_to_g!(num, nf, g)
    fkdiag!(kdiag, g)
    g_num[:, iel] = num
    g_coord[:, num] = coord'
    g_g[:, iel] = g
  end
  println()
  for i in 2:neq
    kdiag[i] = kdiag[i] + kdiag[i-1]
  end
  
  kv = zeros(kdiag[neq])
  gv = zeros(kdiag[neq])

  println("There are $(neq) equations and the skyline storage is $(kdiag[neq]).")
  
  sample!(fin_el, points, weights)
  
  d2x = zeros(neq)
  d2y = zeros(neq)
  d2xy = zeros(neq)
  dtd = zeros(ndof, ndof)
  @assert :thickness in keys(data)
  aa = x_coords[2]-x_coords[1]
  bb = y_coords[2]-y_coords[1]
  
  for iel in 1:nels
    e = prop[etype[iel], 1]
    v = prop[etype[iel], 2]
    d = e * data[:thickness]^3 / (12.0 * (1.0 - v * v))
    g = g_g[:, iel]
    km = zeros(km)
    println()
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
    fsparv!(kv, km, g, kdiag)
  end
  println()
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
  value = zeros(Float64, fixed_freedoms)
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
  
  println("\nNode       Disp            Rot-x         Rot-y           Twist-xy")
  
  tmp = []
  for i in 1:nn
    tmp = vcat(tmp, loads[nf1[:,i]])
    Disp = @sprintf("%+.4e", loads[nf1[1,i]])
    Rotx = @sprintf("%+.4e", loads[nf1[2,i]])
    Roty = @sprintf("%+.4e", loads[nf1[3,i]])
    Twistxy = @sprintf("%+.4e", loads[nf1[4,i]])
    println("  $(i)    $(Disp)     $(Rotx)    $(Roty)     $(Twistxy)")
  end
  #println(round(reshape(float(tmp), 2, 9)', 15))
  
  struc_el.nip = 1
  points = zeros(struc_el.nip, ndim)
  weights = zeros(struc_el.nip)
  sample!(fin_el, points, weights)
  println("\nThe integration point (nip = $(struc_el.nip)) stresses are:")
  if struc_el.axisymmetric
    println("\nElement     sig_r          sig_z            tau_rz            sig_t")
  else
    println("\nElement     sig_x          sig_y            tau_xy")
  end
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
        bm[1] += 4.0*d*(d2x[k]/aa/aa + v*d2y[k]/bb/bb)*loads[g[k]+1]
        bm[2] += 4.0*d*(v*d2x[k]/aa/aa + d2y[k]/bb/bb)*loads[g[k]+1]
        bm[3] += 4.0*d*(1.0-v)*(d2xy[k]/aa/bb*loads[g[k]+1])
      end
      bm1 = @sprintf("%+.4e", bm[1])
      bm2 = @sprintf("%+.4e", bm[2])
      bm3 = @sprintf("%+.4e", bm[3])
      println("   $(iel)     $(bm1)     $(bm2)      $(bm3)")
    end
  end
  for iel in 1:nels
  end
  println()
  bm
end

