using Compat, CSoM

include(Pkg.dir("CSoM", "examples", "StaticEquilibrium", "FE4_1.jl"))

data = @compat Dict(
  # Beam(ndim, nst, nxe, nye, nip, direction, finite_element(nod, nodof), axisymmetric)
  :element_type => Beam(1, 1, 4, 1, :x, Line(2, 1), false),
  :properties => [1.0e5;],
  :x_coords => linspace(0, 1, 5),
  :support => [
    (5, [0])
    ],
  :loaded_nodes => [
    (1, [-0.625]),
    (2, [-1.25]),
    (3, [-1.25]),
    (4, [-1.25]),
    (5, [-0.625])
    ]
)

@time m = FE4_1(data)

@test round(m.displacements, 8) == [-2.5e-5 -2.344e-5 -1.875e-5 -1.094e-5 0.0]'
