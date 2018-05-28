using PtFEM

data = Dict(
# Plane(ndim, nst, nxe, nye, nip, direction=(:x|:y), finite_element(nod, nodof), axisymmetric=false)
# Plane(ndim, nst, nxe, nye, nip, direction=:z, finite_element(nod, nodof), axisymmetric=true)
# Plane(ndim, nst, nre, nze, nip, direction=:r, finite_element(nod, nodof), axisymmetric=false)
  :struc_el => Plane(2, 6, 1, 5, 4, :r, Quadrilateral(8, 3), false),
  :properties => [1.0e5 0.3;],
  :r_coords => [0.0, 0.5],
  :z_coords => [10.0, 8.0, 6.0, 4.0, 2.0, 0.0],
  :support => [
    ( 1, [1 0 1]),
    ( 4, [1 0 1]),
    ( 6, [1 0 1]),
    ( 9, [1 0 1]),
    (11, [1 0 1]),
    (14, [1 0 1]),
    (16, [1 0 1]),
    (19, [1 0 1]),
    (21, [1 0 1]),
    (24, [1 0 1]),
    (26, [0 0 0]),
    (27, [0 0 0]),
    (28, [0 0 0])
    ],
  :loaded_nodes => [
    (3, [1/pi 0.0 0.0])
    ],
  :lth => 1,
  :iflag => 1,
  :chi => 0.0
)

data |> display
println()

@time m, dis_df, fm_df = p52(data)
println()

dis_df |> display
println()

println("\nThe integration point (nip = $(data[:struc_el].nip)) stresses are:")
fm_df |> display
println()
