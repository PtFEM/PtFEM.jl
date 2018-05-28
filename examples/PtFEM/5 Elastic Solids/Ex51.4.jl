using PtFEM

data = Dict(
  # Plane(ndim, nst, nxe, nye, nip, direction, finite_element(nod, nodof), axisymmetric)
  :struc_el => Plane(2, 3, 3, 2, 4, :x, Quadrilateral(8, 2), false),
  :properties => [1.0e6 0.3;],
  :x_coords => [0.0, 3.0, 6.0],
  :y_coords => [0.0, -3.0, -6.0, -9.0],
  :support => [
    ( 1, [0 1]),
    ( 6, [0 1]),
    ( 9, [0 1]),
    (14, [0 1]),
    (17, [0 1]),
    (22, [0 1]),
    (25, [0 0]),
    (26, [0 0]),
    (27, [0 0]),
    (28, [0 0]),
    ( 5, [0 1]),
    ( 8, [0 1]),
    (13, [0 1]),
    (16, [0 1]),
    (21, [0 1]),
    (24, [0 1]),
    (29, [0 0])
    ],
  :loaded_nodes => [
    (1, [0.0 -0.5]),
    (2, [0.0 -2.0]),
    (3, [0.0 -0.5])
    ]
)

data |> display
println()

@time m, dis_df, fm_df = p51(data)
println()

dis_df |> display
println()

println("\nThe integration point (nip = $(data[:struc_el].nip)) stresses are:")
fm_df |> display
println()

