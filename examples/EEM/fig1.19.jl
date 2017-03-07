using CSoM

ProjDir = dirname(@__FILE__)

E = 2.0e7
A = [0.01, 0.02, 0.04]


data = Dict(
  # Frame(nels, nn, ndim, nst, nip, finite_element(nod, nodof))
  :struc_el => Frame(3, 4, 3, 1, 1, Line(2, 3)),
  :properties => [
    E*A[1] 1.0e6 1.0e6 3.0e5;
    E*A[2] 1.0e6 1.0e6 3.0e5;
    E*A[3] 1.0e6 1.0e6 3.0e5;
  ],
  :etype => [1, 2, 3],
  :x_coords => [0.0, 2.0, 3.6, 6.0],
  :y_coords => zeros(4),
  :z_coords => zeros(4),
  :g_num => [
    collect(1:3)';
    collect(2:4)'],
  :support => [
    (4, [0 0 0 0 0 0])
    ],
  :loaded_nodes => [
    (1, [400.0 0.0 0.0 0.0 0.0 0.0]),
    (2, [600.0 0.0 0.0 0.0 0.0 0.0]),
    (3, [800.0 0.0 0.0 0.0 0.0 0.0]),
    (4, [200.0 0.0 0.0 0.0 0.0 0.0])
  ]
)

data |> display
println()

@time m = FE4_4(data)
println()

println("Displacements:")
m.displacements' |> display
println()

println("Actions:")
m.actions' |> display
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
    x_translation = m.displacements[1, :],
  )
  fm_dt = DataTable(
    normal_force_1 = m.actions[1, :],
    normal_force_2 = m.actions[7, :]
  )
    
  display(dis_dt)
  println()
  display(fm_dt)
  
  using Plots
  gr(size=(400,500))

  x = data[:x_coords]
  u = convert(Array, dis_dt[:x_translation])
  N = vcat(
    convert(Vector, fm_dt[:normal_force_2]),
    -2000.0
  )
    
  p = Vector{Plots.Plot{Plots.GRBackend}}(2)
  titles = ["EEM fig 1.1 u(x)", "EEM fig 1.1 N(x)"]
   
  p[1]=plot(N, x, yflip=true, xflip=true, xlab="Normal force [N]",
    ylab="x [m]", color=:blue, leg=false,
    title=titles[2])
    vals = convert(Array, fm_dt[:normal_force_2])

  p[2] = plot(u, x, xlim=(0.0, 0.02), yflip=true,
    xlab="Displacement [m]", ylab="x [m]",
    fillto=0.0, fillalpha=0.1, leg=false, title=titles[1])

  plot(p..., layout=(1, 2))
  savefig(ProjDir*"/fig1.19.png")
  
end
