using CSoM

ProjDir = dirname(@__FILE__)

l = 1.0       # Total length [m]
q = 5.0       # Distributed load [N/m]
N = 8         # Number of nodes
els = N - 1   # Number of finite elements
nod = 2       # Number of nodes per finite elements
nodof = 1     # Degrees of freedom for each node
np_types = 1  # Number of proerty types
EA = 1.0    # Strain stiffness
nip = 1       # Number of integration points

struct_el = :Rod
fin_el = :Line

data = Dict(
  # Rod(nxe, np_types, nip, fin_el(nod, nodof))
  :struc_el => getfield(Main, Symbol(struct_el))(els, np_types, nip,
    getfield(Main, Symbol(fin_el))(nod, nodof)),
  :properties => [EA;],
  :x_coords => 0.0:l/els:l,
  :support => [(N, [0])],
  :loaded_nodes => [(i, repeat([0.0], inner=nodof)) for i in 1:N],
  :eq_nodal_forces_and_moments => [(i, repeat([0.0], inner=nodof*nod)) for i in 1:els]
);

# In this example there are only distributed loads. Otherwise set data[:loaded_nodes]
# to the external forces and moment directly applied to the nodes, e.g.:
#
# data[:loaded_nodes][2] = (2, [5.0])

# Determine the distributed loads contribution to nodal forces

for node in 1:els
  data[:eq_nodal_forces_and_moments][node][2][1] = 1/2*q*l/els
  data[:eq_nodal_forces_and_moments][node][2][2] = 1/2*q*l/els
end  

# Add the equivalent distributed forces and moment to loaded_nodes entry

for node in 1:N-1
  data[:loaded_nodes][node][2][1] += data[:eq_nodal_forces_and_moments][node][2][1]
  data[:loaded_nodes][node+1][2][1] += data[:eq_nodal_forces_and_moments][node][2][2]
end  

data |> display
println()

@time m = p4_1(data)
println()

# Stiffness matrix
println("Stiffness matrix:")
full(sparse(m.cfgsm)) |> display
println()

if VERSION.minor > 5
  println("Displacements:")
  m.displacements' |> display
  println()

  println("Actions:")
  m.actions' |> display
  println()
else
  using DataTables
  dis_dt = DataTable(
    x_translation = m.displacements[:, 1],
  )
  fm_dt = DataTable(
    normal_force_1 = m.actions[:, 1],
    normal_force_2 = m.actions[:, 2],
    normal_force_1_corrected = m.actions[:, 1],
    normal_force_2_corrected = m.actions[:, 2]
  )
  # Correct element forces and moments for equivalent nodal
  # forces and moments introduced for loading between nodes
  
  if :eq_nodal_forces_and_moments in keys(data)
    eqfm = data[:eq_nodal_forces_and_moments]
    k = data[:struc_el].fin_el.nod * data[:struc_el].fin_el.nodof
    for t in eqfm
      vals = convert(Array, fm_dt[t[1], :])
      for i in 1:k
        fm_dt[t[1], i+2] = round(vals[i] - t[2][i], 5)
      end
    end
  end
    
  display(dis_dt)
  println()
  display(fm_dt)
  
  using Plots
  gr(size=(400,500))

  x = 0.0:l/els:l
  u = convert(Array, dis_dt[:x_translation])
  N = vcat(
    convert(Vector, fm_dt[:normal_force_1_corrected])[1],
    convert(Vector, fm_dt[:normal_force_2_corrected])
  )
    
  p = Vector{Plots.Plot{Plots.GRBackend}}(2)
  titles = ["EEM fig 1.1 u(x)", "EEM fig 1.1 N(x)"]
   
  p[1]=plot(ylim=(0.0, 1.0), xlim=(-5.0, 0.0),
    yflip=true, xflip=false, xlab="Normal force [N]",
    ylab="x [m]", title=titles[2]
  )
  vals = convert(Array, fm_dt[:normal_force_2])
  for i in 1:els
      plot!(p[1], 
        [vals[i], vals[i]],
        [(i-1)*l/els, i*l/els], color=:blue,
        color=:blue, fill=true, fillalpha=0.1, leg=false
      )
      delta = abs(((i-1)*l/els) - (i*l/els)) / 20.0
      y1 = collect(((i-1)*l/els):delta:(i*l/els))
      for j in 1:length(y1)
        plot!(p[1],
          [vals[i], 0.0],
          [y1[j], y1[j]], color=:blue, alpha=0.5
        )
      end
  end
  plot!(p[1], N, x, xlim=(-5.0, 0.0), color=:red)
  
  p[2] = plot(u, x, xlim=(0.0, 0.00003), yflip=true,
    xlab="Displacement [m]", ylab="x [m]",
    fillto=0.0, fillalpha=0.1, leg=false, title=titles[1])

  if !isdefined(Main, :defl)
    defl(x) = q/(2*EA).*(l^2 .- x.^2)
  end
  x1 = 0.0:0.01:l
  plot!(p[2], defl(x1), x1, xlim=(0.0, 2.5), color=:red)
  
  plot(p..., layout=(1, 2))
  savefig(ProjDir*"/fig1.1a.png")
  
end
