using CSoM

ProjDir = dirname(@__FILE__)

# Setup loaded nodes entry
function create_loaded_nodes_array()
  lnodes =  Array{Tuple{Int64,Array{Float64,2}},1}()
  push!(lnodes, (25, [-20.0 0.0]))
  push!(lnodes, (50, [-(4.0*0.1)/2.0 (4.0*0.1^2)/12.0]))
  for i in 1:29
    push!(lnodes, (50+i, [-(4.0*0.1)/2.0 0.0]))
  end
  push!(lnodes, (80, [-(4.0*0.1)/2.0 -(4.0*0.1^2)/12.0]))
  last_node = size(lnodes, 1)
  
  # update nodes 80 to 100 with contribution by diminishing load
  for i in 1:20
    push!(lnodes, (80+i, [0.0 0.0]))
  end

  w = 4.0                             # [kN/m]
  l = 2.0                             # [m]
  l_step = 0.1                        # 20 steps
  w_step = 0.2                        # each step w get 0.2 less
  Wr = (w - w_step)*l_step            # area lower rectangle
  Wt = w_step * l_step * 1/2          # add area of triangle
  W = Wr + Wt
  lnodes[last_node][2][1] += -2W/3
  lnodes[last_node][2][2] += -(W*0.1^2)/12.0
  lnodes[last_node+1][2][1] += -W/3

  for i in 1:19
    Wr = (w - (i+1)*w_step)*l_step    # area lower rectangle
    Wt = w_step * l_step * 1/2        # add area of triangle
    W = Wr + Wt
    Vx = 2*(Wr+Wt)/3
    Mx = 2*W*(l-0.0)/(3*sqrt(3.0))*(l^2 - (l-0.1*(20-i))^2)
    lnodes[last_node+i][2][1] += -2*(Wr+Wt)/3
    lnodes[last_node+i][2][2] += -(W*0.1^2)/12.0
    lnodes[last_node+i+1][2][1] += -Wr/3
  end
  lnodes
end

data = Dict(
  # Beam(ndim, nst, nxe, nip, direction, finite_element(nod, nodof), axisymmet
  # ?Beam for more details
  :struc_el => Beam(2, 1, 100, 1, :x, Line(2, 2), false),
  :properties => [4.0e4; 2.0e4],
  :etype => repeat(1:2, inner=50),
  :x_coords => 0.0:1.0:100.0,
  :support => [
    (1, [0 1]),
    (80, [0 1])
    ],
  :loaded_nodes => create_loaded_nodes_array(),
  :fixed_freedoms => [
    (1, 2, -0.001),
    (50, 1, -0.005)],
  :penalty => 1e19
)

data |> display
println()

@time m = FE4_3(data)
println()

if VERSION.minor > 5
  println("Displacements:")
  m.displacements' |> display
  println()

  println("Actions:")
  m.actions' |> display
  println()
else
  using DataFrames
  df = DataFrame(
    xl_Force = m.actions[1, :],
    xl_Moment = m.actions[2, :],
    xr_Force = m.actions[3, :],
    xr_Moment = m.actions[4, :]
  )
  #display(df)
  
  using Plots
  gr(size=(400,600))

  p = Vector{Plots.Plot{Plots.GRBackend}}(3)
  p[1] = plot(m.x_coords, m.displacements[2,:], ylim=(-0.1, 0.1), lab="y Displacement", 
   xlabel="x [m]", ylabel="deflection [m]", color=:red)
  p[2] = plot(m.actions[2,:], lab="Shear force", ylim=(-150, 170), xlabel="element",
    ylabel="shear force [N]", palette=:greens,fill=(0,:auto),α=0.6)
  p[3] = plot(m.actions[4,:], lab="Moment", ylim=(-150, 170), xlabel="element",
    ylabel="moment [Nm]", palette=:grays,fill=(0,:auto),α=0.6)

  plot(p..., layout=(3, 1))
  savefig(ProjDir*"/figure.4.16.png")
  
end

