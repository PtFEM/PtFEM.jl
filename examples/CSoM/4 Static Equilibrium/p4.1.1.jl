using CSoM

ProjDir = dirname(@__FILE__)

l = 1.0       # Total length [m]
N = 5         # Number of nodes
els = N - 1   # Number of finite elements
nod = 2       # Number of nodes per finite elements
nodof = 1     # Degrees of freedom for each node
np_types = 1  # Number of proerty types
EA = 1.0e5    # Strain stiffness
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
  :loaded_nodes => [
      (1, [-0.625]),
      (2, [-1.25]),
      (3, [-1.25]),
      (4, [-1.25]),
      (5, [-0.625])
    ]
);

data |> display
println()

@time m = FE4_1(data)
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
    normal_force_2 = m.actions[:, 2]
  )
    
  display(dis_dt)
  println()
  display(fm_dt)
  
  using Plots
  gr(size=(400,500))

  x = 0.0:l/els:l
  u = convert(Array, dis_dt[:x_translation])
  Nf = vcat(
    convert(Array, fm_dt[:normal_force_1])[1],
    convert(Array, fm_dt[:normal_force_2])
  )
    
  p = Vector{Plots.Plot{Plots.GRBackend}}(2)
  titles = ["CSoM p4.1.1 u(x)", "CSoM p4.1.1 N(x)"]
   
  p[1]=plot(ylim=(0.0, 1.0), xlim=(0.0, 5.0),
    yflip=true, xflip=true, xlab="Normal force [N]",
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
  
  p[2] = plot(u, x, xlim=(-0.00003, 0.0), yflip=true,
    xlab="Displacement [m]", ylab="x [m]",
    fillto=0.0, fillalpha=0.1, leg=false, title=titles[1])

  plot(p..., layout=(1, 2))
  savefig(ProjDir*"/p4.1.1.png")
  
end

F = 10.0
data[:loaded_nodes] = [(i, [0.0]) for i in 1:N]
for node in 1:N-1
  data[:loaded_nodes][node][2][1] += F / (2els)
  data[:loaded_nodes][node+1][2][1] += F / (2els)
end  

@time m2 = FE4_1(m, data)
println()

if VERSION.minor > 5
  println("Displacements:")
  m2.displacements' |> display
  println()

  println("Actions:")
  m2.actions' |> display
  println()
else
  using DataTables
  dis_dt2 = DataTable(
    x_translation = m2.displacements[:, 1],
  )
  fm_dt2 = DataTable(
    normal_force_1 = m2.actions[:, 1],
    normal_force_2 = m2.actions[:, 2]
  )
    
  display(dis_dt2)
  println()
  display(fm_dt2)
end