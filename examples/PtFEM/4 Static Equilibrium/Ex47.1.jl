using PtFEM

ProjDir = dirname(@__FILE__)
exname = split(@__FILE__)[end][1:end-3]

data = Dict(
  # Plane(ndim, nst, nxe, nye, nip, direction, finite_element(nod, nodof), axisymmetric)
  :struc_el => Plane(2, 3, 2, 2, 16, :x, Quadrilateral(4, 4), false),
  :properties => [10.92 0.3;],
  :x_coords => range(0.0, stop=0.5, lenght=3),
  :y_coords => range(0.0, stop=0.5, length=3),
  :thickness => 1.0,
  :support => [
    (1, [0 0 0 1]),
    (2, [0 0 1 1]),
    (3, [0 0 1 0]),
    (4, [0 1 0 1]),
    (6, [1 0 1 0]),
    (7, [0 1 0 0]),
    (8, [1 1 0 0]),
    (9, [1 0 0 0])
    ],
  :loaded_nodes => [
    (9, [0.25 0.0 0.0 0.0])
    ]
)

data |> display
println()

@time dis_df, sigma_df = p47(data)
println()

PtFEM.vtk(data, dis_df, sigma_df, ProjDir, "Ex47.1")
println()

dis_df |> display
println()
println("\nThe integration point (nip = $(data[:struc_el].nip)) stresses are:")
sigma_df |> display