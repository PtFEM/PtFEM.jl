"""
# Method p63_skyline

Plane strain bearing capacity analysis of an elastic-plastic (Mohr-Coulomb) material
using 8-node rectangular quadrilaterals. Rigid smooth footing. Displacement control.
Viscoplastic strain method.

### Constructors
```julia
p63_skyline(data)
```
### Arguments
```julia
* `data::Dict{Symbol, Any}` : Dictionary containing all input data
```

### Required data dictionary keys
```julia
* struc_el::StructuralElement                          : Structural element
* properties::Vector{Float64}                          : Material properties
* x_coords::FloatRange{Floalt64}                       : x-coordinate vector
* y_coords::FloatRange{Floalt64}                       : y-coordinate vector
```

### Optional additional data dictionary keys
```julia
* tol::Float64                 : Convergence tolerance
* limit = 250                  : Iteration limit
* incs::Int                    : Incremental load steps
* presc::Float64               : Wall displacement increment
* penalty = 1e20               : Penalty used for fixed degrees of freedoms
* etype::Vector{Int}           : Element material vector if np_types > 1
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
function p63_skyline(data::Dict)
  
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
    (nels, nn) = PtFEM.mesh_size(fin_el, struc_el.nxe)
  elseif typeof(fin_el) == Triangle || typeof(fin_el) == Quadrilateral
    (nels, nn) = PtFEM.mesh_size(fin_el, struc_el.nxe, struc_el.nye)
  elseif typeof(fin_el) == Hexahedron
    (nels, nn) = PtFEM.mesh_size(fin_el, struc_el.nxe, struc_el.nye, struc_el.nze)
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
  
  PtFEM.bc_rect!(struc_el.nxe, struc_el.nye, nf, struc_el.direction)
  neq = maximum(nf)
  kdiag = zeros(Int, neq)
  
  #
  # Program 6.3 specific variables
  #
  
  tol = 1.0e-10
  if :tol in keys(data)
    tol = copy(data[:tol])
  end
  
  limit = 100
  if :limit in keys(data)
    limit = copy(data[:limit])
  end
  
  @assert :incs in keys(data)
  incs = copy(data[:incs])
  
  @assert :nbo2 in keys(data)
  nbo2 = copy(data[:nbo2])
  
  @assert :qs in keys(data)
  qs = copy(data[:qs])
  
  @assert :presc in keys(data)
  presc = copy(data[:presc])
  
  # Find global array sizes
  for iel in 1:nels
    PtFEM.geom_rect!(fin_el, iel, collect(x_coords), collect(y_coords), coord, num,
      struc_el.direction)
    PtFEM.num_to_g!(num, nf, g)
    g_num[:, iel] = num
    g_coord[:, num] = coord'
    g_g[:, iel] = g
    PtFEM.fkdiag!(kdiag, g)
  end
  
  for i in 2:neq
    kdiag[i] = kdiag[i] + kdiag[i-1]
  end
  
  kv = zeros(kdiag[neq])
  gv = zeros(kdiag[neq])

  println("There are $(neq) equations and the skyline storage is $(kdiag[neq]).")
  
  start_df = 1.0e15
  dt = copy(start_df)
  
  for i in 1:size(data[:properties], 1)
    ϕ, c, ψ, γ, E, ν  = prop[i, :]
    snph = sind(ϕ)
    ddt = 4.0 * (1.0 + ν) * ( 1.0 - 2.0ν) / (E * (1.0 - 2.0ν + snph^2))
    ddt < dt && (dt = ddt)
    #println([i ddt dt])
  end
  
  gravlo = zeros(Float64, neq+1)
    
  PtFEM.sample!(fin_el, points, weights)
  
  for iel in 1:nels
    PtFEM.deemat!(dee, prop[etype[iel], 5], prop[etype[iel], 6])
    num = g_num[:, iel]
    coord = g_coord[:, num]'              # Transpose
    g = g_g[:, iel]
    km = zeros(ndof, ndof)
    eld = zeros(ndof)
    for i in 1:struc_el.nip
      PtFEM.shape_fun!(fun, points, i)
      PtFEM.shape_der!(der, points, i)
      jac = der*coord
      detm = det(jac)
      jac = inv(jac)
      deriv = jac*der
      PtFEM.beemat!(bee, deriv)
      km += (bee')*dee*bee*detm*weights[i]
      eld[2:2:ndof] += fun .* detm * weights[i]
    end
    PtFEM.fsparv!(kv, km, g, kdiag)
    gravlo[g+1] -= eld .* prop[etype[iel], 4] 
  end
  
  kvc = deepcopy(kv)
  
  # Surcharge loads
  
  nf1 = deepcopy(nf) + 1
  for i in 1:struc_el.nxe
    i3 = g_num[3, (i-1)*struc_el.nye+1]
    i4 = g_num[4, (i-1)*struc_el.nye+1]
    i5 = g_num[5, (i-1)*struc_el.nye+1]
    qq = (x_coords[i+1] - x_coords[i]) * qs
    gravlo[nf1[2, i3]] -= qq/6.0
    gravlo[nf1[2, i4]] -= 2.0qq/3.0
    gravlo[nf1[2, i5]] -= qq/6.0
  end
  
  PtFEM.sparin!(kv, kdiag)
  gravlo[2:end] = PtFEM.spabac!(kv, gravlo[2:end], kdiag)
  gravlo[1] = 0.0
  
  tensor = zeros(nst, struc_el.nip, nels)
  
  for iel in 1:nels
    PtFEM.deemat!(dee, prop[etype[iel], 5], prop[etype[iel], 6])
    g = g_g[:, iel]
    eld = gravlo[g+1]
    num = g_num[:, iel]
    coord = g_coord[:, num]'              # Transpose
    for i in 1:struc_el.nip
      PtFEM.shape_der!(der, points, i)
      jac = der*coord
      jac = inv(jac)
      deriv = jac*der
      PtFEM.beemat!(bee, deriv)
      sigma = dee*(bee*eld)
      for j in 1:4
        tensor[j, i, iel] = sigma[j]
      end
    end
  end
  
  fixed_freedoms = 2nbo2 + 1
  node = zeros(Int, fixed_freedoms)
  no = zeros(Int, fixed_freedoms)
  storkv = zeros(Float64, fixed_freedoms)
  node[1] = 1
  k = 1
  
  for i in 1:nbo2
    k += 2*struc_el.nye + 1
    node[2*i] = k
    k += struc_el.nye + 1
    node[2*i + 1] = k
  end
  
  kv = deepcopy(kvc)
  for i in 1:fixed_freedoms
    no[i] = nf[2, node[i]]
  end
  kv = kvc
  kv[kdiag[no]] += penalty
  storkv = kv[kdiag[no]]
  
  sparin!(kv, kdiag)

  local iters, loads, bdylds, converged
  oldis = zeros(Float64, neq+1)
  totd = zeros(Float64, neq+1)
  
  println("\nstep   disp    load1    load2     iters\n")

  disps = Array{Float64, 1}()
  loads1 = Array{Float64, 1}()
  loads2 = Array{Float64, 1}()
  iterations = Array{Int, 1}()
  
  for iy in 1:incs
    iters = 0
    bdylds = zeros(Float64, neq+1)
    react = zeros(Float64, neq+1)
    evpt = zeros(nst, struc_el.nip, nels)
    
    while true
      iters += 1
      
      loads = zeros(Float64, neq+1)
      loads += bdylds
      
      for i in 1:fixed_freedoms
        loads[no[i]+1] = storkv[i] * presc
      end

      loads[2:end] = spabac!(kv, loads[2:end], kdiag)
      
      converged = checon(loads, oldis, tol)
      iters == 1 && (converged = false)
      
      if converged || iters == limit
        bdylds = zeros(Float64, neq+1)
      end
  
      sigm = 0.0
      dsbar = 0.0
      lode_theta = 0.0
      f = 0.0
      dq1 = 0.0
      dq2 = 0.0
      dq3 = 0.0
      for iel in 1:nels
        ϕ, c, ψ, γ, E, ν  = prop[etype[iel], :]
        deemat!(dee, E, ν)
        bload = zeros(ndof)
        rload = zeros(ndof)
        num = g_num[:, iel]
        coord = g_coord[:, num]'
        g = g_g[:, iel]
        eld = loads[g+1]
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
          f = mocouf(ϕ, c, sigm, dsbar, lode_theta)
          if converged || iters == limit
            devp = deepcopy(stress)
          else
            if f >= 0.0
              dq1, dq2, dq3 = mocouq(ϕ, dsbar, lode_theta)
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
            bload += eload' * detm * weights[i]
          end
          if converged || iters == limit
            tensor[:,i,iel] = stress
            rload += (stress' * bee)' * detm * weights[i]
          end
        end
        bdylds[g+1] += bload
        react[g+1] += rload
        bdylds[1] = 0.0
      end
      (converged || iters == limit) && break
    end
    totd += loads
    pr = 0.0
    for i in 1:fixed_freedoms
      pr += react[no[i]+1]
    end
    pr /= x_coords[nbo2+1]
    pav = 0.0
    for i in 1:nbo2
      pav += tensor[2, 1, (i-1)*struc_el.nye+1] + tensor[2, 2, (i-1)*struc_el.nye+1]
    end
    pav /= 2nbo2
    append!(disps, -totd[2])
    append!(loads1, -pr)
    append!(loads2, -pav)
    append!(iterations, iters)
    println("$(iy)     $(-round.(totd[2], 5))   $(-round.(pr, 5)) $(-round.(pav, 5))    $(iters)")
    iters == limit && continue
  end
  
  println()
  displacements = zeros(size(nf))
  for i in 1:size(displacements, 1)
    for j in 1:size(displacements, 2)
      if nf[i, j] > 0
        displacements[i,j] = totd[nf[i, j]+1]
      end
    end
  end
  
  res_df = DataFrame(
    displacement = disps,
    load1 = loads1,
    load2 = loads2,
    iters = iterations
  )
  
  (res_df, g_coord, g_num, displacements')
end

