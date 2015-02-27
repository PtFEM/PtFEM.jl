using Compat, CSoM, Base.Test

include(Pkg.dir("CSoM", "examples", "StaticEquilibrium", "FE4_6.jl"))

data = @compat Dict(
  # Beam(ndim, nst, nxe, nip, direction, finite_element(nod, nodof), axisymmetric)
  :element_type => Beam(2, 1, 4, 1, :x, Line(2, 1), false),
  :properties => [1.0;],
  :x_coords => [0.0, 0.25, 0.5, 0.75, 1.0],
  :support => [
    (1, [0 1]),
    (5, [0 0])
    ],
  :limit => 100,
  :tol => 0.00001
)

@time m = FE4_6(data)

@test round(m[4], 5) == [0.0, 0.72734, 0.15237, 0.38835, 0.16868, -0.24397, 0.06725, -0.45216]
