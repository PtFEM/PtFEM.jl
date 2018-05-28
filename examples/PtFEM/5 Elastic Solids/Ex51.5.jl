using PtFEM

data = Dict(
  # Plane(ndim, nst, nxe, nye, nip, direction, finite_element(nod, nodof), axisymmetric)
  :struc_el => Plane(2, 3, 3, 2, 9, :z, Quadrilateral(4, 2), true),
  :properties => [
     100.0 0.3;
    1000.0 0.45
    ],
  :etype => [1, 2, 1, 2, 1, 2],
  :x_coords => [0.0, 4.0, 10.0, 30.0],
  :y_coords => [0.0, -4.0, -10.0],
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
  :loaded_nodes => [
    (1, [0.0  -2.6667]),
    (4, [0.0 -23.3333]),
    (7, [0.0 -24.0])
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
