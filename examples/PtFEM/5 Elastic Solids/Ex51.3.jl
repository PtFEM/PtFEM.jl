using PtFEM

data = Dict(
  # Plane(ndim, nst, nxe, nye, nip, direction, finite_element(nod, nodof), axisymmetric)
  :struc_el => Plane(2, 3, 3, 2, 4, :y, Quadrilateral(4, 2), false),
  :properties => [1.0e6 0.3;],
  :x_coords => [0.0, 10.0, 20.0, 30.0],
  :y_coords => [0.0, -5.0, -10.0],
  :support => [
    (1, [0 1]),
    (2, [0 1]),
    (3, [0 0]),
    (6, [0 0]),
    (9, [0 0]),
    (10, [0 1]),
    (11, [0 1]),
    (12, [0 0])
    ],
  :fixed_freedoms => [
    (1, 2, -1.0e-5),
    (4, 2, -1.0e-5)
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
