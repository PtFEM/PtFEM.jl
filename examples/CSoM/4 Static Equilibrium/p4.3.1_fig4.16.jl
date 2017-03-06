using CSoM

ProjDir = dirname(@__FILE__)

data = Dict(
  # Beam(ndim, nst, nxe, nip, direction, finite_element(nod, nodof), axisymmet
  # ?Beam for more details
  :struc_el => Beam(2, 1, 9, 1, :x, Line(2, 2), false),
  :properties => [4.0e4; 2.0e4],
  :etype => [1,1,2,2,2,2,2,2,2],
  :x_coords => [0.0, 2.5, 5.0, 5.5, 6.0, 6.5, 7.0, 7.5, 8.0, 10.0],
  :support => [
    (1, [0 1]),
    (9, [0 1])
    ],
  :fixed_freedoms => [
    (1, 2, -0.001),
    (3, 1, -0.005)],
  :loaded_nodes => [
      (2, [-20.0 0.0])
      (3, [-1.0 -0.02083])
      (4, [-1.0 0.0])
      (5, [-1.0 0.0])
      (6, [-1.0 0.0])
      (7, [-1.0 0.0])
      (8, [-1.0 0.0])
      (9, [-3.8 -0.77917])
      (10, [-1.2 0.5333])
    ],
  :penalty => 1e19,
  :eq_nodal_forces_and_moments => [
    (3, [-1.0 -0.02083 -1.0 0.02083]),
    (4, [-1.0 -0.02083 -1.0 0.02083]),
    (5, [-1.0 -0.02083 -1.0 0.02083]),
    (6, [-1.0 -0.02083 -1.0 0.02083]),
    (7, [-1.0 -0.02083 -1.0 0.02083]),
    (8, [-1.0 -0.02083 -1.0 0.02083]),
    (9, [-2.8 -0.8 -1.2  0.5333])
  ]
)

data[:loaded_nodes]

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
  dis_df = DataFrame(
    y_translation = round(m.displacements'[:, 1], 5),
    z_rotation = round(m.displacements'[:, 2], 5)
  )
  fm_df = DataFrame(
    y1_Force = round.(m.actions[1, :], 2),
    z1_Moment = round.(m.actions[2, :], 2),
    y2_Force = round.(m.actions[3, :], 2),
    z2_Moment = round.(m.actions[4, :], 2)
  )
  # Correct element forces and moments for equivalent nodal
  # forces and moments introduced for loading between nodes
  if :eq_nodal_forces_and_moments in keys(data)
    eqfm = data[:eq_nodal_forces_and_moments]
    k = data[:struc_el].fin_el.nod * data[:struc_el].fin_el.nodof

    for t in eqfm
      for i in 1:k
        fm_df[t[1], i] -= round(t[2][i], 2)
      end
    end
  end
    
  display(dis_df)
  println()
  display(fm_df)
  
  using Plots
  gr(size=(400,600))

  p = Vector{Plots.Plot{Plots.GRBackend}}(3)
  titles = ["p4.3.1 y deflection", "p4.3.1 y shear force", "p4.3.1 z moment"]
  fors = vcat(fm_df[:, :y1_Force], fm_df[end, :y2_Force])
  moms = vcat(fm_df[:, :z1_Moment], fm_df[end, :z2_Moment])
  
  p[1] = plot(m.displacements[2,:], ylim=(-0.005, 0.005),
    xlabel="node", ylabel="deflection [m]", color=:red,
    line=(:dash,1), marker=(:circle,4,0.8,stroke(1,:black)),
    title=titles[1], leg=false)
  p[2] = plot(fors, ylim=(-20.0, 25),
    xlabel="node", ylabel="y shear force [N]", color=:blue,
    line=(:dash,1), marker=(:circle,4,0.8,stroke(1,:black)),
    title=titles[2], leg=false)
  p[3] = plot(moms, ylim=(-40, 40),
    xlabel="node", ylabel="z moment [Nm]", color=:green,
    line=(:dash,1), marker=(:circle,4,0.8,stroke(1,:black)),
    title=titles[3], leg=false)

  plot(p..., layout=(3, 1))
  savefig(ProjDir*"/p4.3.1_fig4.16.png")
  
end

