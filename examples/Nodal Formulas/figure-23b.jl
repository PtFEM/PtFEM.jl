using PtFEM

ProjDir = dirname(@__FILE__)

data = Dict(
  # Beam(ndim, nst, nxe, nip, direction, finite_element(nod, nodof), axisymmet
  :struc_el => Beam(2, 1, 20, 1, :x, Line(2, 2), false),
  :properties => [1.0; 1.0],
  #:etype => [1, 1, 2, 2],
  :x_coords => 0.0:1.0:20.0,
  :support => [
      (1, [0 0]),
      (21, [0 0])
    ],
  :loaded_nodes => [(i, [-1.0 0.0]) for i in 1:20],
  :penalty => 1e19
)

data |> display
println()

@time m, dis_df, fm_df = p43(data)
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
end

if VERSION.minor == 5
  using Plots
  gr(size=(400,600))

  p = Vector{Plots.Plot{Plots.GRBackend}}(undef, 3)
  p[1] = plot(m.x_coords, m.displacements[1,:], ylim=(-500.0, 10.0), lab="Displacement", 
   xlabel="x [m]", ylabel="deflection [m]", color=:red)
  p[2] = plot(m.actions'[:,1], lab="Shear force", ylim=(-15, 15), xlabel="element",
    ylabel="shear force [N]", palette=:greens,fill=(0,:auto),α=0.6)
  p[3] = plot(m.actions'[:,4], lab="Moment", ylim=(-30, 35), xlabel="element",
    ylabel="moment [Nm]", palette=:grays,fill=(0,:auto),α=0.6)

  plot(p..., layout=(3, 1))
  savefig(ProjDir*"/figure-23b.png")
end
