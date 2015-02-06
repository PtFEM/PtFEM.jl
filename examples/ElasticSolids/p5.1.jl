using Compat, CSoM

#=
'plane'
'triangle'   3   'x'
2  2  1
1
1.0e6  0.3
0.0  0.5  1.0
0.0 -0.5 -1.0
5
1 0 1  4 0 1  7 0 0  8 1 0  9 1 0
3
1  0.0 -0.25   2  0.0 -0.50   3  0.0 -0.25
0
=#

data = @compat Dict(
  :elementtype => Plane(2, 2, 1, :x, Triangle(3)),
  :nproperties => 1,
  :properties => [1.0e6 0.3; 0.0 0.0],
  :etype => [],
  :x_coords => [0.0,  0.5,  1.0],
  :y_coords => [0.0,  -0.5,  -1.0],
  :support => [
    (1, [0 1]),
    (4, [0 1]),
    (7, [0 0]),
    (8, [1 0]),
    (9, [1 0])
    ],
  :loads => [
    (1, [0.0 -0.25]),
    (2, [0.0 -0.50]),
    (3, [0.0 -0.25])
    ],
  :fixed_freedoms =>[]
)

data |> display
println()

@time m = FEmodel(data)
println()
