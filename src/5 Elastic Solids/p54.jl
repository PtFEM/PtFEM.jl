function p54(data::Dict)
  
  # Setup basic dimensions of arrays
  
  # Parse & check FEdict data
  
  if :struc_el in keys(data)
    struc_el = data[:struc_el]
    @assert typeof(struc_el) == GenericSolid
  else
    println("No struc_el type specified.")
    return
  end
  
  ndim = struc_el.ndim
  nst = struc_el.nst
  nels = struc_el.nels
  nn = struc_el.nn
  
  fin_el = struc_el.fin_el
  @assert typeof(fin_el) <: FiniteElement
  
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
  y_coords = zeros(nn)
  z_coords = zeros(nn)
  
  g_coord = zeros(ndim,nn)
  @assert :g_coord in keys(data)
  g_coord = data[:g_coord]'
  
  g_num = zeros(Int, fin_el.nod, nels)
  @assert :g_num in keys(data)
  g_num = reshape(data[:g_num]', fin_el.nod, nels)

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
    num = g_num[:, iel]
    num_to_g!(num, nf, g)
    g_g[:, iel] = g
  end
  
  sample!(fin_el, points, weights)
  
  loads = OffsetArray(zeros(neq + 1), 0:neq)
  gravlo = OffsetArray(zeros(neq + 1), 0:neq)
  
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
      beemat!(bee, deriv)
      km += (bee')*dee*bee*detm*weights[i]
      eld[nodof:nodof:ndof] += fun[:]*detm*weights[i]
    end
    fsparm!(gsm, g, km)
    gravlo[g] -= eld*prop[etype[iel], 3]
  end
  
  if :loaded_nodes in keys(data)
    for i in 1:size(data[:loaded_nodes], 1)
      loads[nf[:, data[:loaded_nodes][i][1]]] = data[:loaded_nodes][i][2]
    end
  end
  loads += gravlo
  
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

  if ndim == 3
    dis_df = DataFrame(
      x_disp = displacements[:, 1],
      y_disp = displacements[:, 2],
      z_disp = displacements[:, 3]
    )
  else
    dis_df = DataFrame(
      x_disp = displacements[:, 1],
      y_disp = displacements[:, 2]
    )
  end
  
  loads[0] = 0.0
  
  gc1 = Vector{Float64}()
  gc2 = Vector{Float64}()
  s1 = Vector{Float64}()
  s2 = Vector{Float64}()
  s3 = Vector{Float64}()
  
  if ndim ==  3
    gc3 = Vector{Float64}()
    t1 = Vector{Float64}()
    t2 = Vector{Float64}()
    t3 = Vector{Float64}()
  end    
  
  for iel in 1:nels
    deemat!(dee, prop[etype[iel], 1], prop[etype[iel], 2])
    num = g_num[:, iel]
    coord = g_coord[:, num]'
    g = g_g[:, iel]
    eld = loads[g]
    for i in 1:struc_el.nip
      shape_der!(der, points, i)
      shape_fun!(fun, points, i)
      gc = fun'*coord
      jac = inv(der*coord)
      deriv = jac*der
      beemat!(bee, deriv)
      sigma = dee*(bee*eld)
      gc1 = append!(gc1, gc[1])
      gc2 = append!(gc2, gc[2])
      s1 = append!(s1, sigma[1])
      s2 = append!(s2, sigma[2])
      s3 = append!(s3, sigma[3])
      if ndim == 3
        gc3 = append!(gc3, gc[3])
        t1 = append!(t1, sigma[4])
        t2 = append!(t2, sigma[5])
        t3 = append!(t3, sigma[6])
      end
    end
  end
  
  if ndim == 3
    sigma_df = DataFrame(
      x_coord = gc1,
      y_coord = gc2,
      z_coord = gc3,
      sig_x = s1,
      sig_y = s2,
      sig_z = s3,
      tau_xy = t1,
      tau_yz = t2,
      tau_zx = t3
    )
  else
    println("\nElement  x-coord   y-coord      sig_x        sig_y        tau_xy")
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

