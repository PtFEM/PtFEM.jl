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
  p[1] = plot(m.x_coords, m.displacements[2,:], ylim=(-0.005, 0.005), lab="y Displacement", 
   xlabel="x [m]", ylabel="deflection [m]", color=:red)
  x_coords = data[:x_coords]
  moms = vcat(fm_df[:, :xl_Moment], fm_df[end, :xr_Moment])
  fors = vcat(fm_df[:, :xl_Force], fm_df[end, :xr_Force])
  p[2] = plot(x_coords, fors, lab="Shear force", ylim=(-20.0, 25), xlabel="element",
    ylabel="shear force [N]", palette=:greens,fill=(0,:auto),α=0.6)
  p[3] = plot(x_coords, moms, lab="Moment", ylim=(-40, 40), xlabel="element",
    ylabel="moment [Nm]", palette=:grays,fill=(0,:auto),α=0.6)

  plot(p..., layout=(3, 1))
  savefig(ProjDir*"/p4.3.1_fig.4.16.png")
  
end

