using PtFEM

data = Dict(
  # Plane(ndim, nst, nxe, nye, nip, direction, finite_element(nod, nodof), axisymmetric)
  :struc_el => Plane(2, 3, 2, 1, 12, :x, Triangle(15, 2), false),
  :properties => [1.0e5 0.2;],
  :x_coords => [0.0,  1.0,  6.0],
  :y_coords => [0.0,  -2.0],
  :support => [
    ( 1, [0 1]),
    ( 9, [0 1]),
    (10, [0 1]),
    (18, [0 1]),
    (19, [0 1]),
    (27, [0 1]),
    (28, [0 1]),
    (36, [0 1]),
    (37, [0 0]),
    (38, [0 0]),
    (39, [0 0]),
    (40, [0 0]),
    (41, [0 0]),
    (42, [0 0]),
    (43, [0 0]),
    (44, [0 0]),
    (45, [0 0])
    ],
  :loaded_nodes => [
    (1, [0.0 -7/90]),
    (2, [0.0 -16/45]),
    (3, [0.0 -2/15]),
    (4, [0.0 -16/45]),
    (5, [0.0 -7/90])
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
