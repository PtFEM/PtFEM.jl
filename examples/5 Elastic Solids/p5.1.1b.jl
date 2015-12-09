using Compat, CSoM

include("FE5_1.jl")

data = @compat Dict(
  # Plane(ndim, nst, nxe, nye, nip, direction, finite_element(nod, nodof), axisymmetric)
  :element_type => Plane(2, 3, 2, 2, 12, :x, Triangle(6, 2), false),
  :properties => [1.0e6 0.3;],
  :x_coords => [0.0,  0.5,  1.0],
  :y_coords => [0.0, -0.5, -1.0],
  :support => [
    ( 1, [0 1]),
    (11, [0 1]),
    (21, [0 0]),
    (23, [1 0]),
    (25, [1 0])
    ],
  :loaded_nodes => [
    (1, [0.0 -1/12]),
    (2, [0.0 -2/6]),
    (3, [0.0 -1/6]),
    (4, [0.0 -2/6]),
    (5, [0.0 -1/12])
    ]
)

data |> display
println()

@time m = FE5_1(data)
println()
