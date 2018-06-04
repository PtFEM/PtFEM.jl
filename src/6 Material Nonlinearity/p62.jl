"""
# Method p62 

Plane strain bearing capacity analysis of an elastic-plastic
(von Mises) material using 8-node rectangular quadrilaterals.

Viscoplastic strain method.

No global stiffness matrix assembly.

Diagonally preconditioned conjugate gradient solver.

### Constructors
```julia
p62(data)
```
### Arguments
```julia
* `data::Dict{Symbol, Any}` : Dictionary containing all input data
```

### Required data dictionary keys
```julia
* struc_el::StructuralElement                          : Structural element
* support::Array{Tuple{Int,Array{Int,1}},1}        : Fixed-displacements vector
* loaded_nodes::Array{Tuple{Int,Array{Float64,1}},1} : Node load vector
* properties::Vector{Float64}                          : Material properties
* x_coords::FloatRange{Floalt64}                       : x-coordinate vector
* y_coords::FloatRange{Floalt64}                       : y-coordinate vector
* thickness:: Float64                                  : Thickness of plate
* tol::Float64                                         : Convergence tolerance
* qincs::Vector{Float64}                               : Incremental load steps
```

### Optional additional data dictionary keys
```julia
* limit = 250                  : Iteration limit
* penalty = 1e20               : Penalty used for fixed degrees of freedoms
* etype::Vector{Int}         : Element material vector if np_types > 1
```

### Return values
```julia
* (g_coord, g_num, disp)        : where:
                                    g_coord  : Coordinates
                                    g_num    : Node numbering
                                    disp     : Matrix of displacements
```

### Related help
```julia
?StructuralElement             : List of available structural element types
?Plane                         : Help on a Plane structural element
?FiniteElement                 : List finite element types
?Quadrilateral                 : Help on Quadrilateral finite element
```
"""
function p62(data::Dict)
  
  # Setup basic dimensions of arrays
  
  # Parse & check FEdict data
  
  if :struc_el in keys(data)
    struc_el = data[:struc_el]
    @assert typeof(struc_el) <: StructuralElement
  else
    println("No fin_el type specified.")
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
  
  tol = 1.0e-10
  if :tol in keys(data)
    tol = copy(data[:tol])
  end
  
  limit = 100
  if :limit in keys(data)
    limit = copy(data[:limit])
  end
    
  if :cg_tol in keys(data)
    cg_tol = data[:cg_tol]
  else
    cg_tol = 1.0e-5
  end
  
  if :cg_limit in keys(data)
    cg_limit = data[:cg_limit]
  else
    cg_limit = Int(1e3)
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
  
  eps = zeros(nst)
  stress = zeros(nst)
  bload =zeros(ndof)
  eload =zeros(ndof)
  erate = zeros(nst)
  evp = zeros(nst)
  devp = zeros(nst)
  m1 = zeros(nst, nst)
  m2 = zeros(nst, nst)
  m3 = zeros(nst, nst)
  flow = zeros(nst, nst)
  
  formnf!(nodof, nn, nf)
  neq = maximum(nf)
  
  @assert :qincs in keys(data)
  qincs = data[:qincs]
  
  println("There are $(neq) equations.")
  
  loads = zeros(Float64, neq+1)
  bdylds = zeros(Float64, neq+1)
  oldis = zeros(Float64, neq+1)
  totd = zeros(Float64, neq+1)
  p = zeros(nst, struc_el.nip, nels)
  x = zeros(neq+1)
  u = zeros(neq+1)
  xnew = zeros(neq+1)
  diag_precon = zeros(neq+1)
  d = zeros(neq+1)
  
  # Find global array sizes
  for iel in 1:nels
    geom_rect!(fin_el, iel, x_coords, y_coords, coord, num, struc_el.direction)
    num_to_g!(num, nf, g)
    g_num[:, iel] = num
    g_coord[:, num] = coord'
    g_g[:, iel] = g
  end

  tensor = zeros(nst, struc_el.nip, nels)
  storkm = zeros(ndof, ndof, nels)
  #tload = zeros(neq+1)
  #dtemp = zeros(nn)
  #dtel = zeros(fin_el.nod)
  #epsi = zeros(nst)
  dt = 1.0e15
  ddt = 0.0
  sigm = 0.0
  dsbar = 0.0
  lode_theta = 0.0
  
  sample!(fin_el, points, weights)
  
  # element stiffnes integration, storage and conditioner
  
  for iel in 1:nels
    ddt = 4.0*(1.0+prop[etype[iel], 3])/(3.0*prop[etype[iel], 2])
    if ddt < dt
      dt = ddt
    end
    deemat!(dee, prop[etype[iel], 2], prop[etype[iel], 3])
    num = g_num[:, iel]
    coord = g_coord[:, num]'              # Transpose
    g = g_g[:, iel]
    km = zeros(ndof, ndof)
    for i in 1:struc_el.nip
      shape_der!(der, points, i)
      jac = der*coord
      detm = det(jac)
      jac = inv(jac)
      deriv = jac*der
      beemat!(bee, deriv)
      km += (bee')*dee*bee*detm*weights[i]
    end
    storkm[:,:,iel] = km
    for k in 1:ndof
      diag_precon[g[k]+1] = diag_precon[g[k]+1] + km[k,k]
    end
  end
  
  diag_precon = 1 ./ diag_precon
  
  # Read load weightings
  
  loaded_nodes = 0
  node = Int[]
  val = Array{Float64, 2}
  if :loaded_nodes in keys(data)
    loaded_nodes = size(data[:loaded_nodes], 1)
    node = zeros(Int, loaded_nodes)
    val = zeros(loaded_nodes, size(data[:loaded_nodes][1][2], 2))
  end
  
  if :loaded_nodes in keys(data)
    for i in 1:loaded_nodes
      node[i] = data[:loaded_nodes][i][1]
      val[i,:] = data[:loaded_nodes][i][2]
    end
  end

  nf1 = deepcopy(nf) .+ 1
  
  # Load increment loop
  
  #println("\nstep     load        disp       iters    cg/plastic iters")

  converged = false
  ptot = 0.0
  iters = 0
  cg_tot = 0
  diag_precon[1] = 0.0
  iy = 0
  
  # DataFrame arrays
  
  loaddt = Float64[]
  dispdt = Float64[]
  itersdt = Int[]
  ratiodt = Float64[]
  
  #for iy in 1:1
  for iy in 1:size(qincs, 1)
    ptot = ptot + qincs[iy]
    iters = 0
    bdylds = zeros(Float64, neq+1)
    evpt = zeros(nst, struc_el.nip, nels)
    cg_tot = 0
    cg_converged = false
    
    # Plastic iteration loop
    
    while true
      iters += 1
      loads = zeros(Float64, neq+1)
      for i in 1:loaded_nodes
        loads[nf1[:,node[i]]] = val[i,:] * qincs[iy]
      end
      loads += bdylds
      d = diag_precon .* loads
      p = d
      x = zeros(neq+1)
      cg_iters = 0
      
      # Pcg equation solution
      tmpconvcrit = 0.0
      
      while true
        cg_iters += 1
        u = zeros(neq+1)
        
        for iel in 1:nels
          g = g_g[:, iel]
          km = storkm[:, :, iel]
          u[g .+ 1] += km * p[g .+ 1]
        end
        up = dot(loads, d)
        alpha = up ./ dot(p, u)
        xnew = x + p .* alpha
        loads -= u .* alpha
        d = diag_precon .* loads
        beta = dot(loads, d) ./ up
        p = d + p * beta
        #tmpconvcrit =  maximum(abs.(xnew-x))/maximum(abs.(xnew))
        cg_converged = checon(xnew, x, cg_tol)
        #iy < 2 && println([iters cg_iters cg_tot+cg_iters tmpconvcrit])
        if cg_converged || (cg_iters == cg_limit)
          break
        end
      end
      cg_tot += cg_iters
      loads = xnew
      loads[1] = 0.0
      
      # Check plastic convergence
      
      tmp = maximum(abs.(loads-oldis))/maximum(abs.(loads))
      converged = checon(loads, oldis, tol)
      if iters == 1
        converged = false
      end
      if converged || (iters == limit)
        bdylds = zeros(Float64, neq+1)
      end
      
      # Go round the Gauss point
      
      for iel in 1:nels
        deemat!(dee, prop[etype[iel], 2], prop[etype[iel], 3])
        num = g_num[:, iel]
        coord = g_coord[:, num]'
        g = g_g[:, iel]
        eld = loads[g .+ 1]
        bload = zeros(ndof)
        for i in 1:struc_el.nip
          shape_der!(der, points, i)
          jac = der*coord
          detm = det(jac)
          jac = inv(jac)
          deriv = jac*der
          beemat!(bee, deriv)
          eps = bee*eld
          eps -= evpt[:, i, iel]
          sigma = dee*eps
          stress = sigma + tensor[:, i, iel]
          (sigm, dsbar, lode_theta) = invar(stress, sigm, dsbar, lode_theta)
          
          # Check whether yield is violated
          
          f = dsbar - sqrt(3.0)*prop[etype[iel], 1]
          if converged || iters == limit
            devp = deepcopy(stress)
          else
            if f >= 0.0
              dq1 = 0.0
              dq2 = 3.0/2.0/dsbar
              dq3 = 0.0
              (m1, m2, m3) = formm!(stress, m1, m2, m3)
              flow = f*(m1*dq1 + m2*dq2 + m3*dq3)
              erate = flow*stress
              evp = erate*dt
              evpt[:,i,iel] += evp
              devp = dee*evp
            end
          end
          if f >= 0.0 || converged || iters == limit
            eload = devp' * bee
            bload += eload' .* detm .* weights[i]
          end
          
          # Update the Gauss point stresses
          
          if converged || iters == limit
            tensor[:,i,iel] = stress
          end
        end
        
        # Compute the total bodyloads vector
        
        bdylds[g .+ 1] += bload
        bdylds[1] = 0.0
      end
      if converged || iters == limit
        break
      end
    end
    totd += loads
    totdstr = @sprintf("%+.4e", totd[nf1[2, node[1]]])
    
    append!(loaddt, [ptot])
    append!(dispdt, [totd[nf1[2, node[1]]]])
    append!(itersdt, [iters])
    append!(ratiodt, [round.(cg_tot/iters, digits=2)])
    
    if iy < 10
      println(" $(iy)       $(ptot)    $(totdstr)    $(iters)       $(round.(cg_tot/iters, digits=2))")
    else
      println("$(iy)       $(ptot)    $(totdstr)    $(iters)       $(round.(cg_tot/iters, digits=2))")
    end
    
  end
  println()
  
  dt = DataFrame()
  dt[:loads] = loaddt
  dt[:disp] = dispdt
  dt[:iters] = itersdt
  dt[:ratio] = ratiodt
  dt
end

