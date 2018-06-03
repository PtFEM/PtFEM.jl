using PtFEM, Test

data = Dict(
  # Beam(ndim, nst, nxe, nip, direction, finite_element(nod, nodof), axisymmetric)
  :struc_el => Beam(2, 1, 4, 1, :x, Line(2, 1), false),
  :properties => [1.0;],
  :x_coords => [0.0, 0.25, 0.5, 0.75, 1.0],
  :support => [
    (1, [0 1]),
    (5, [0 0])
    ],
  :limit => 100,
  :tol => 0.00001
)

@time m = p46(data)

@test m[4] ≈ [0.0,0.7273399696355276,0.15236981309765138,0.3883541804128791,
  0.1686800077576337,-0.24396646116150877,0.06725268705915496,
  -0.4521565393506495]