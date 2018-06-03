function p55(data::Dict)
  
  # Setup basic dimensions of arrays
  
  # Parse & check FEdict data
  
  if :struc_el in keys(data)
    struc_el = data[:struc_el]
    @assert typeof(struc_el) <: StructuralElement
  else
    println("No struc_el type specified.")
    return
  end
  
  ndim = struc_el.ndim
  nst = struc_el.nst
  
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

  g_coord = zeros(ndim,nn)
  if :g_coord in keys(data)
    g_coord = data[:g_coord]'
  end
  
  g_num = zeros(Int, fin_el.nod, nels)
  if :g_num in keys(data)
    g_num = reshape(data[:g_num]', fin_el.nod, nels)
  end

  etype = ones(Int, nels)
  if :etype in keys(data)
    etype = data[:etype]
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
  
  # Find global array sizes
  for iel in 1:nels
    geom_rect!(fin_el, iel, x_coords, y_coords, coord, num, struc_el.direction)
    num_to_g!(num, nf, g)
    g_num[:, iel] = num
    g_coord[:, num] = coord'
    g_g[:, iel] = g
  end
  
  sample!(fin_el, points, weights)
  
  loads = OffsetArray(zeros(neq + 1), 0:neq)
    
  teps = zeros(nst)
  tload = OffsetArray(zeros(neq + 1), 0:neq)
  dtemp = zeros(nn)
  dtel = zeros(fin_el.nod)
  epsi = zeros(nst)
  
  if :dtemp in keys(data)
    dtemp = data[:dtemp]
  end
  
  gsm = spzeros(neq, neq)
  for iel in 1:nels
    deemat!(dee, prop[etype[iel], 1], prop[etype[iel], 2])
    num = g_num[:, iel]
    coord = g_coord[:, num]'              # Transpose
    g = g_g[:, iel]
    dtel = dtemp[num]
    etl = zeros(ndof)
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
        gc = fun * coord
        bee[4, 1:2:(ndof-1)] = fun[:]/gc[1]
      end
      km += (bee')*dee*bee*detm*weights[i]*gc[1]
      gtemp = dot(fun, dtel)
      teps[1:2] = gtemp*prop[etype[iel], 3:4]
      etl += (bee')*dee*teps*detm*weights[i]*gc[1]
    end
    tload[g] += etl
    fsparm!(gsm, g, km)
  end
  
  nspr = 0
  if :nspr in keys(data)
    nspr = size(data[:nspr], 1)
  end
  
  sno = zeros(Int, nspr)
  spno = zeros(Int, nspr)
  spse = zeros(Int, nspr)
  spva = zeros(Float64, nspr)
  if :nspr in keys(data) && nspr > 0
    for i in 1:nspr
      spno[i] = data[:nspr][i][1]
      spse[i] = data[:nspr][i][2]
      spva[i] = data[:nspr][i][3]
      sno[i] = nf[spse[i], spno[i]]
      gsm[sno[i], sno[i]] += spva[i]
    end
  end

  if :loaded_nodes in keys(data)
    for i in 1:size(data[:loaded_nodes], 1)
      loads[nf[:, data[:loaded_nodes][i][1]]] = data[:loaded_nodes][i][2]
    end
  end
  loads += loads + tload
  
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

  if struc_el.axisymmetric
    dis_df = DataFrame(
      r_disp = displacements[:, 1],
      z_disp = displacements[:, 2]
    )
  else
    dis_df = DataFrame(
      x_disp = displacements[:, 1],
      y_disp = displacements[:, 2]
    )
  end
  
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
  if struc_el.axisymmetric
    s4 = Vector{Float64}()
  end
  
  for iel in 1:nels
    deemat!(dee, prop[etype[iel], 1], prop[etype[iel], 2])
    num = g_num[:, iel]
    coord = g_coord[:, num]'
    g = g_g[:, iel]
    eld = loads[g]
    dtel = dtemp[num]
    for i in 1:struc_el.nip
      shape_fun!(fun, points, i)
      shape_der!(der, points, i)
      gc = fun'*coord
      jac = inv(der*coord)
      deriv = jac*der
      beemat!(bee, deriv)
      if struc_el.axisymmetric
        gc = fun * coord
        bee[4, 1:2:(ndof-1)] = fun[:]/gc[1]
      end
      gtemp = dot(fun, dtel)
      teps[1:2] = gtemp*prop[etype[iel], 3:4]
      epsi = bee*eld - teps
      sigma = dee*epsi
      gc1 = append!(gc1, gc[1])
      gc2 = append!(gc2, gc[2])
      s1 = append!(s1, sigma[1])
      s2 = append!(s2, sigma[2])
      s3 = append!(s3, sigma[3])
      if struc_el.axisymmetric
        s4 = append!(s4, sigma[4])
      end
    end
  end

  if struc_el.axisymmetric
     sigma_df = DataFrame(
      r_coord = gc1,
      z_coord = gc2,
      sig_r = s1,
      sig_z = s2,
      tau_rz = s3,
      sig_t = s4
    )
  else
    sigma_df = DataFrame(
      x_coord = gc1,
      y_coord = gc2,
      sig_x = s1,
      sig_y = s2,
      tau_xy = s3
    )
  end
  
  fem = PtFEM.jFEM(struc_el, fin_el, ndim, nels, nst, ndof, nn, nodof,
    neq, penalty, etype, g, g_g, g_num, nf, no,
    node, num, sense, actions, bee, coord, gamma, dee,
    der, deriv, displacements, eld, fun, gc, g_coord, jac,
    km, mm, kg, cfgsm, loads, points, prop, sigma, value,
    weights, x_coords, y_coords, z_coords, axial)
  
    (fem, dis_df, sigma_df)
  end

