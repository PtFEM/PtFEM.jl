using PtFEM

data = Dict(
  # Plane(ndim, nst, nxe, nye, nip, direction, finite_element(nod, nodof), axisymmetric)
  :struc_el => Plane(2, 3, 2, 2, 6, :x, Triangle(3, 2), false),
  :properties => [1.0e6 0.3;],
  :x_coords => [0.0,  0.5,  1.0],
  :y_coords => [0.0,  -0.5,  -1.0],
  :support => [
    (1, [0 1]),
    (4, [0 1]),
    (7, [0 0]),
    (8, [1 0]),
    (9, [1 0])
    ],
  :loaded_nodes => [
    (1, [0.0 -0.25]),
    (2, [0.0 -0.50]),
    (3, [0.0 -0.25])
    ]
)

println()
data |> display
println()

@time m = p51_skyline(data)
println()
