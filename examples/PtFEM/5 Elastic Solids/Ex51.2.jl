using PtFEM

data = Dict(
  # Plane(ndim, nst, nxe, nye, nip, direction, finite_element(nod, nodof), axisymmetric)
  :struc_el => Plane(2, 3, 2, 1, 12, :y, Triangle(15, 2), false),
  :properties => [1.0e5 0.2;],
  :x_coords => [0.0,  1.0,  6.0],
  :y_coords => [0.0,  -2.0],
  :support => [
    (1, [0 1]),
    (2, [0 1]),
    (3, [0 1]),
    (4, [0 1]),
    (5, [0 0]),
    (10, [0 0]),
    (15, [0 0]),
    (20, [0 0]),
    (25, [0 0]),
    (30, [0 0]),
    (35, [0 0]),
    (40, [0 0]),
    (41, [0 1]),
    (42, [0 1]),
    (43, [0 1]),
    (44, [0 1]),
    (45, [0 0])
    ],
  :loaded_nodes => [
    (1, [0.0 -7/90]),
    (6, [0.0 -16/45]),
    (11, [0.0 -2/15]),
    (16, [0.0 -16/45]),
    (21, [0.0 -7/90])
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
