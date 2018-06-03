using PtFEM, Test

data = Dict(
  # Plane(ndim, nst, nxe, nye, nip, direction, finite_element(nod, nodof), axisymmetric)
  :struc_el => Plane(2, 3, 2, 2, 16, :x, Quadrilateral(4, 4), false),
  :properties => [10.92 0.3;],
  :x_coords => collect(range(0.0, stop=0.5, length=3)),
  :y_coords => collect(range(0.0, stop=0.5, length=3)),
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


@time fm_df, sigma_df = p47(data)

@test round.(convert(Array,sigma_df[4,:]), digits=4)' ≈ [-0.1211,-0.1211,-0.0335]