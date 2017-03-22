using CSoM

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

@time m = p4_4(data)
println()

using DataTables
dis_dt = DataTable(
  x_translations = m.displacements[1, :],
  y_translations = m.displacements[2, :],
  rotations = m.displacements[3, :]
)
fm_dt = DataTable(
  x1_Force = m.actions[1, :],
  y1_Force = m.actions[2, :],
  z1_Moment = m.actions[3, :],
  x2_Force = m.actions[4, :],
  y2_Force = m.actions[5, :],
  z2_Moment = m.actions[6, :]
)
# Correct element forces and moments for equivalent nodal
# forces and moments introduced for loading between nodes
if :eq_nodal_forces_and_moments in keys(data)
  eqfm = data[:eq_nodal_forces_and_moments]
  k = data[:struc_el].fin_el.nod * data[:struc_el].fin_el.nodof
  for t in eqfm
    vals = convert(Array, fm_dt[t[1], :])
    for i in 1:k
      fm_dt[t[1], i] = round(vals[i] - t[2][i], 2)
    end
  end
end
  
display(dis_dt)
println()
display(fm_dt)
  
if VERSION.minor < 6
  using Plots
  gr(size=(400,600))

  p = Vector{Plots.Plot{Plots.GRBackend}}(4)
  titles = ["p4.4.1 translations", "p4.4.1 rotations",
    "p4.4.1 y shear force", "p4.4.1 z moment"]
  indxs = [1,2,3,4,5,6,7,9,10,11,12,13,14,16,17]
  moms = vcat(
    convert(Array, fm_dt[:, :z1_Moment]), 
    convert(Array, fm_dt[:, :z2_Moment])[end]
  )
  fors = vcat(
    convert(Array, fm_dt[:, :y1_Force]), 
    convert(Array, fm_dt[:, :y2_Force])[end]
  )
  x_coords = data[:x_coords]
  
  indxs = [1,2,3,4,5,6,7,9,10,11,12,13,14,16,17]
  p[1] = plot(m.displacements[2,indxs], ylim=(-0.015, 0.015),
    xlabel="node", ylabel="y translation [m]", color=:black,
    line=(:dash,1), marker=(:circle,4,0.8,stroke(1,:black)),
    title=titles[1], leg=false)
  p[2] = plot(m.displacements[3,indxs], ylim=(-0.008, 0.008),
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
  savefig(ProjDir*"/Ex4.4.1a.png")
  
end
