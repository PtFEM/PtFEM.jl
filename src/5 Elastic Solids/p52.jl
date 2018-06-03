function p52(data::Dict)
  
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
  
  # Handle :r direction (implicit axisymmetric)
  if struc_el.direction == :r
    nre = struc_el.nxe::Int
    nze = struc_el.nye::Int
  end
  
  # Add radial stress
  if struc_el.axisymmetric
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
    println("No :properties key found in FEdict.")
    exit(1)
  end
    
  nf = ones(Int, nodof, nn)
  if :support in keys(data)
    for i in 1:size(data[:support], 1)
      nf[:, data[:support][i][1]] = data[:support][i][2]
    end
  else
    println("No :supports key found in FEdict.")
    exit(1)
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

  r_coords = zeros(nn)
  if :r_coords in keys(data)
    r_coords = data[:r_coords]
  end

  etype = ones(Int, nels)
  if :etype in keys(data)
    etype = data[:etype]
  end
  
  @assert :lth in keys(data)
  @assert :iflag in keys(data)
  @assert :chi in keys(data)
  lth = data[:lth]::Int
  iflag = data[:iflag]::Int
  chi = data[:chi]*pi/180.0
  ca = cos(chi)
  sa = sin(chi)
  radius = 0.0
  
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
  println("There are $(neq) equations.\n")
  
  # Find global array sizes
  
  for iel in 1:nels
    geom_rect!(fin_el, iel, r_coords, z_coords, coord, num, struc_el.direction)
    num_to_g!(num, nf, g)
    g_num[:, iel] = num
    g_coord[:, num] = coord'
    g_g[:, iel] = g
  end
  
  sample!(fin_el, points, weights)

  gsm = spzeros(neq, neq)
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
      (radius, bee) = bmat_nonaxi!(bee, radius, coord, deriv, fun, iflag, lth)
      km += (((bee')*dee)*bee)*detm*weights[i]*radius
    end
    fsparm!(gsm, g, km)
  end
  
  loads = OffsetArray(zeros(neq + 1), 0:neq)
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
      loads[no[i]] = gsm[no[i], no[i]] * value[i]
    end
  end
  
  cfgsm = cholesky(gsm)
  loads[1:neq] = cfgsm \ loads[1:neq]

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
    r_disp = displacements[:, 1],
    z_disp = displacements[:, 2],
    t_disp = displacements[:, 3]
  )
  
  loads[0] = 0.0
  
  struc_el.nip = 1
  points = zeros(struc_el.nip, ndim)
  weights = zeros(struc_el.nip)
  
  sample!(fin_el, points, weights)

  gc1 = Vector{Float64}()
  gc2 = Vector{Float64}()
  s1 = Vector{Float64}()
  s2 = Vector{Float64}()
  s3 = Vector{Float64}()
  t1 = Vector{Float64}()
  t2 = Vector{Float64}()
  t3 = Vector{Float64}()
  
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
      jac = inv(der*coord)
      deriv = jac*der
      (radius, bee) = bmat_nonaxi!(bee,radius,coord,deriv,fun,iflag,lth)
      bee[1:4,:]=bee[1:4,:]*ca 
      bee[5:6,:]=bee[5:6,:]*sa
      sigma = dee*(bee*eld)
      gc1 = append!(gc1, gc[1])
      gc2 = append!(gc2, gc[2])
      s1 = append!(s1, sigma[1])
      s2 = append!(s2, sigma[2])
      s3 = append!(s3, sigma[3])
      t1 = append!(t1, sigma[4])
      t2 = append!(t2, sigma[5])
      t3 = append!(t3, sigma[6])
    end
  end
  
  sigma_df = DataFrame(
    r_coord = gc1,
    z_coord = gc2,
    sig_r = s1,
    sig_z = s2,
    sig_t = s3,
    tau_rz = t1,
    tau_zt = t2,
    tau_tr = t3
  )
  
  fem = PtFEM.jFEM(struc_el, fin_el, ndim, nels, nst, ndof, nn, nodof,
    neq, penalty, etype, g, g_g, g_num, nf, no,
    node, num, sense, actions, bee, coord, gamma, dee,
    der, deriv, displacements, eld, fun, gc, g_coord, jac,
    km, mm, kg, cfgsm, loads, points, prop, sigma, value,
    weights, x_coords, y_coords, z_coords, axial)
  
    (fem, dis_df, sigma_df)
end

