using Compat, CSoM

include("FE4_6.jl")

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

data |> display
println()

@time m = FE4_6(data)
println()

println("\nThe buckling load = $(m[1])")
m[4][1] = 0.0
println("\nThe buckling mode (iterations=$(m[2])):\n")
for i in 1:m[3]
  println("$i    $(m[4][m[5][:, i]+1])")
end
