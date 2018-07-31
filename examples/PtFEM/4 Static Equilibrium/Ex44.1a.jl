using PtFEM

ProjDir = dirname(@__FILE__)

x_coords = 0.0:1.0:6.0
#[0.0, 6.0, 6.0, 12.0, 12.0, 14.0],
y_coords = repeat(0.0:0.0, outer=7) 
#0.0, -4.0, 0.0, -5.0, 0.0],
x_coords = vcat(x_coords, 6.0)
y_coords = vcat(y_coords, -4.0)
x_coords = vcat(x_coords, collect(7.0:1.0:12.0))
y_coords = vcat(y_coords, repeat(0.0:0.0, outer=6))
x_coords = vcat(x_coords, 12.0)
y_coords = vcat(y_coords, -5.0)
x_coords = vcat(x_coords, 13.0:1.0:14.0)
y_coords = vcat(y_coords, repeat(0.0:0.0, outer=2))

xy_coords = hcat(x_coords, y_coords)

data = Dict(
  # Frame(nels, nn, ndim, nst, nip, finite_element(nod, nodof))
  :struc_el => Frame(17, 17, 2, 1, 1, Line(2, 3)),
  :properties => [
      5.0e9 6.0e4;
      1.0e9  2.0e4
    ],
  :etype => vcat(repeat(1:1, outer=14), repeat(2:2, outer=3)),
  :x_coords => x_coords,
  :y_coords => y_coords,
  :g_num => [
      1 2 3 4 5 6 7  9 10 11 12 13 14 16 8  8 15;
      2 3 4 5 6 7 9 10 11 12 13 14 16 17 7 14 14
    ],
  :support => [
      ( 1, [0 0 1]),
      ( 8, [0 0 0]),
      (15, [0 0 0])
    ],
  :loaded_nodes => [
      ( 1, [0.0 -10.0 -20//12]),
      ( 2, [0.0 -20.0     0.0]),
      ( 3, [0.0 -20.0     0.0]),
      ( 4, [0.0 -20.0     0.0]),
      ( 5, [0.0 -20.0     0.0]),
      ( 6, [0.0 -20.0     0.0]),
      ( 7, [0.0 -20.0     0.0]),
      ( 9, [0.0 -20.0     0.0]),
      (10, [0.0 -80.0     0.0]),
      (11, [0.0 -20.0     0.0]),
      (12, [0.0 -80.0     0.0]),
      (13, [0.0 -20.0     0.0]),
      (14, [0.0 -20.0     0.0]),
      (16, [0.0 -20.0     0.0]),
      (17, [0.0 -10.0  20//12])
    ],
  :penalty => 1e19,
  :eq_nodal_forces_and_moments => [
    ( 1, [0.0 -10.0 -20//12 0.0 -10.0 20//12]),
    ( 2, [0.0 -10.0 -20//12 0.0 -10.0 20//12]),
    ( 3, [0.0 -10.0 -20//12 0.0 -10.0 20//12]),
    ( 4, [0.0 -10.0 -20//12 0.0 -10.0 20//12]),
    ( 5, [0.0 -10.0 -20//12 0.0 -10.0 20//12]),
    ( 6, [0.0 -10.0 -20//12 0.0 -10.0 20//12]),
    ( 7, [0.0 -10.0 -20//12 0.0 -10.0 20//12]),
    ( 9, [0.0 -10.0 -20//12 0.0 -10.0 20//12]),
    (10, [0.0 -10.0 -20//12 0.0 -10.0 20//12]),
    (11, [0.0 -10.0 -20//12 0.0 -10.0 20//12]),
    (12, [0.0 -10.0 -20//12 0.0 -10.0 20//12]),
    (13, [0.0 -10.0 -20//12 0.0 -10.0 20//12]),
    (14, [0.0 -10.0 -20//12 0.0 -10.0 20//12]),
    (16, [0.0 -10.0 -20//12 0.0 -10.0 20//12]),
    (17, [0.0 -10.0 -20//12 0.0 -10.0 20//12])
  ]
)

data |> display
println()

@time fem, dis_df, fm_df = p44(data)
println()

display(dis_df)
println()
display(fm_df)
  
if VERSION.minor < 6
  using Plots
  gr(size=(400,600))

  p = Vector{Plots.Plot{Plots.GRBackend}}(undef, 4)
  titles = ["p4.4.1 translations", "p4.4.1 rotations",
    "p4.4.1 y shear force", "p4.4.1 z moment"]
  indxs = [1,2,3,4,5,6,7,9,10,11,12,13,14,16,17]
  moms = vcat(
    convert(Array, fm_df[:, :z1_Moment]), 
    convert(Array, fm_df[:, :z2_Moment])[end]
  )
  fors = vcat(
    convert(Array, fm_df[:, :y1_Force]), 
    convert(Array, fm_df[:, :y2_Force])[end]
  )
  x_coords = data[:x_coords]
  
  indxs = [1,2,3,4,5,6,7,9,10,11,12,13,14,16,17]
  p[1] = plot(fem.displacements[2,indxs], ylim=(-0.015, 0.015),
    xlabel="node", ylabel="y translation [m]", color=:black,
    line=(:dash,1), marker=(:circle,4,0.8,stroke(1,:black)),
    title=titles[1], leg=false)
  p[2] = plot(fem.displacements[3,indxs], ylim=(-0.008, 0.008),
    xlabel="node", ylabel="rotation [radians]", color=:red,
    line=(:dash,1), marker=(:circle,4,0.8,stroke(1,:black)),
    title=titles[2], leg=false)
  p[3] = plot(fors, lab="y Shear force", ylim=(-250.0, 250.0),
    xlabel="node", ylabel="shear force [N]", color=:blue,
    line=(:dash,1), marker=(:circle,4,0.8,stroke(1,:black)),
    title=titles[3], leg=false)
  p[4] = plot(moms, lab="z Moment", ylim=(-150.0, 150.0),
    xlabel="node", ylabel="z moment [Nm]", color=:green,
    line=(:dash,1), marker=(:circle,4,0.8,stroke(1,:black)),
    title=titles[4], leg=false)

  plot(p..., layout=(4, 1))
  savefig(ProjDir*"/Ex44.1a.png")
  
end
