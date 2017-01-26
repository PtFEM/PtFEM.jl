using CSoM

data = Dict(
  # Beam(ndim, nst, nxe, nip, direction, finite_element(nod, nodof), axisymmetric)
  :struc_el => Beam(2, 1, 4, 1, :x, Line(2, 1), false),
  :properties => [1.0 800.0;],
  :x_coords => [0.0, 0.25, 0.5, 0.75, 1.0],
  :support => [
    (1, [0 1]),
    (5, [0 1])
    ],
  :limit => 100,
  :tol => 0.00001
)

data |> display
println()

@time m = FE4_6(data)
println()
