using PtFEM

data = Dict(
  # Frame(nels, nn, ndim, nst, nip, finite_element(nod, nodof))
  :struc_el => Frame(7, 8, 2, 1, 1, Line(2, 3)),
  :properties => [
    1.0e10 1.0e6 20.0;
    1.0e10 1.0e6 50.0;
    1.0e10 1.0e6 80.0
     ],
  :etype => [1, 2, 2, 1, 3, 3, 1],
  :x_coords => [0.0,  0.0, 10.0, 20.0, 20.0, 35.0, 50.0, 50.0],
  :y_coords => [0.0, 15.0, 15.0, 15.0,  0.0, 15.0, 15.0,  0.0],
  :g_num => [
    1 2 3 5 4 6 8;
    2 3 4 4 6 7 7],
  :support => [
    (1, [0 0 0]),
    (5, [0 0 0]),
    (8, [0 0 0])
    ],
  :loaded_nodes => [
    (2, [4.0   0.0 0.0]),
    (3, [0.0  -6.0 0.0]),
    (6, [0.0 -12.0 0.0]),
    ],
  :limit => 250,
  :tol => 0.0001,
  :incs => 8,
  :dload => [0.5; 0.3; 0.2; 0.2; 0.1; 0.05; 0.02; 0.01]
)

data |> display
println()

@time m = p45(data)
println()
