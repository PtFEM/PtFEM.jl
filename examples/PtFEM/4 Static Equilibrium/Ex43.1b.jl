using PtFEM

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

@time fem, dis_df, fm_df = p43(data)
println()

display(dis_df)
println()
display(fm_df)
  
if VERSION.minor < 6
  using Plots
  gr(size=(400,600))

  p = Vector{Plots.Plot{Plots.GRBackend}}(undef, 3)
  titles = ["p43.1 y deflection", "p43.1 y shear force", "p43.1 z moment"]
  fors = vcat(
    convert(Array, fm_df[:, :xl_Force]), 
    convert(Array, fm_df[:, :xr_Force])[end]
  )
  moms = vcat(
    convert(Array, fm_df[:, :xl_Moment]),
    convert(Array, fm_df[:, :xr_Moment])[end]
  )
  
  p[1] = plot(fem.displacements[2,:], ylim=(-0.005, 0.005),
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
  savefig(ProjDir*"/Ex43.1b.png")

end

