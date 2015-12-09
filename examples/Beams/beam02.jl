using Compat, CSoM

include(Pkg.dir("CSoM", "examples", "StaticEquilibrium", "FE4_4.jl"))

data = @compat Dict(
  # Frame(nels, nn, ndim, nst, nip, finite_element(nod, nodof))
  :element_type => Frame(20, 21, 3, 1, 1, Line(2, 3)),
  :properties => [2.0e6 1.0e6 1.0e6 3.0e5;],
  :x_coords => collect(linspace(0, 4, 21)),
  :y_coords => zeros(21),
  :z_coords => zeros(21),
  :g_num => [
    collect(1:20)';
    collect(2:21)'],
  :support => [
    (1, [0 0 0 0 0 0]),
    (21, [0 0 0 0 0 0]),
    ],
  :loaded_nodes => [
    (11, [0.0 -10000.0 0.0 0.0 0.0 0.0])]
)

data |> display
println()

m = FE4_4(data)

println("Displacements:")
m.displacements |> display
println()

println("Actions:")
m.actions |> display
println()

