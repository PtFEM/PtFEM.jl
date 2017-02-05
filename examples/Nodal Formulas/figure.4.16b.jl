using CSoM

data = Dict(
  # Beam(ndim, nst, nxe, nip, direction, finite_element(nod, nodof), axisymmet
  :struc_el => Beam(2, 1, 4, 1, :x, Line(2, 2), false),
  :properties => [4.0e4; 2.0e4],
  :etype => [1, 1, 2, 2],
  :x_coords => [0.0, 2.5, 5.0, 8.0, 10.0],
  :support => [
      (1, [0 1]),
      (4, [0 1])
    ],
  :loaded_nodes => [
      (2, [-20.0 0.0]),
      (3, [-6.0 -3.0]),
      (4, [-8.8 2.2]),
      (5, [-1.2 0.5333])
    ],
  :fixed_freedoms => [
      (1, 2, -0.001),
      (3, 1, -0.005)
    ],
  :penalty => 1e19,
  :eq_nodal_forces_and_moments => [
    (3, [-6.0 -3.0 -6.0 3.0]),
    (4, [-2.8 -0.8 -1.2  0.5333])
  ]
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
  dis_df = DataFrame(
    translations = m.displacements'[:, 1],
    rotations = m.displacements'[:, 2]
  )
  fm_df = DataFrame(
    xl_Force = m.actions[1, :],
    xl_Moment = m.actions[2, :],
    xr_Force = m.actions[3, :],
    xr_Moment = m.actions[4, :]
  )
  # Correct element forces and moments for equivalent nodal
  # forces and moments introduced for loading between nodes
  if :eq_nodal_forces_and_moments in keys(data)
    eqfm = data[:eq_nodal_forces_and_moments]
    k = data[:struc_el].fin_el.nod * data[:struc_el].fin_el.nodof

    for t in eqfm
      for i in 1:k
        fm_df[t[1], i] -= t[2][i]
      end
    end
  end
    
  display(dis_df)
  println()
  display(fm_df)

  using Plots
  gr(size=(400,600))

  p = Vector{Plots.Plot{Plots.GRBackend}}(3)
  p[1] = plot(m.x_coords, m.displacements[2,:], ylim=(-0.01, 0.01), lab="y Displacement", 
   xlabel="x [m]", ylabel="deflection [m]", color=:red)
  moms = vcat(fm_df[:, :xl_Moment], fm_df[end, :xr_Moment])
  fors = vcat(fm_df[:, :xl_Force], fm_df[end, :xr_Force])
  p[2] = plot(fors, lab="Shear force", ylim=(-5, 30), xlabel="element",
    ylabel="shear force [N]", palette=:greens,fill=(0,:auto),α=0.6)
  p[3] = plot(moms, lab="Moment", ylim=(-30, 35), xlabel="element",
    ylabel="moment [Nm]", palette=:grays,fill=(0,:auto),α=0.6)

  plot(p..., layout=(3, 1))
  savefig(ProjDir*"/figure.4.16b.png")
end

