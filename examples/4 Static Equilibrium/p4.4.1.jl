using CSoM

data = Dict(
  # Frame(nels, nn, ndim, nst, nip, finite_element(nod, nodof))
  :struc_el => Frame(6, 6, 2, 1, 1, Line(2, 3)),
  :properties => [
    5.0e9 6.0e4;
    1.0e9  2.0e4],
  :etype => [1, 1, 1, 2, 2, 2],
  :x_coords => [0.0, 6.0, 6.0, 12.0, 12.0, 14.0],
  :y_coords => [0.0, 0.0, -4.0, 0.0, -5.0, 0.0],
  :g_num => [
    1 2 4 3 3 5;
    2 4 6 2 4 4],
  :support => [
    (1, [0 0 1]),
    (3, [0 0 0]),
    (5, [0 0 0])
    ],
  :loaded_nodes => [
    (1, [0.0 -60.0 -60.0]),
    (2, [0.0 -180.0 -80.0]),
    (4, [0.0 -140.0 133.33]),
    (6, [0.0 -20.0 6.67])
    ],
  :penalty => 1e19
)

data |> display
println()

@time m = FE4_4(data)
println()

println("Displacements:")
m.displacements' |> display
println()

println("Actions:")
m.actions' |> display
println()

