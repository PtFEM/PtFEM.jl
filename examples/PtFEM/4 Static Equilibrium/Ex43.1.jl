using PtFEM

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

@time fem, dis_df, fm_df = p43(data)
println()

display(dis_df)
println()
display(fm_df)

