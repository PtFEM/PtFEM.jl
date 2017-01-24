using CSoM

data = Dict(
  # Frame(nels, nn, ndim, nst, nip, finite_element(nod, nodof))
  :element_type => Frame(30, 31, 3, 1, 1, Line(2, 3)),
  :properties => [2.0e6 1.0e6 1.0e6 3.0e5;],
  :x_coords => linspace(0, 4, 31),
  :y_coords => zeros(31),
  :z_coords => zeros(31),
  :g_num => [
    collect(1:30)';
    collect(2:31)'],
  :support => [
    ( 1, [0 0 0 0 0 1]),
    #(11, [1 0 0 0 0 1]),
    (21, [1 0 0 0 0 1]),
    (31, [1 0 0 0 0 1])
    ],
  :loaded_nodes => [
    [( 1, [0.0 -250.0 0.0 0.0 0.0 0.0])];
    [( i, [0.0 -500.0 0.0 0.0 0.0 0.0]) for i in 2:30];
    [(31, [0.0 -250.0 0.0 0.0 0.0 0.0])]
  ]
)

data |> display
println()

@time m = FE4_4(data)
println()

println("Displacements:")
m.displacements |> display
println()

println("Actions:")
m.actions |> display
println()
