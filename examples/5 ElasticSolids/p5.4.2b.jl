using Compat, CSoM

include("FE5_4.jl")

data = @compat Dict(
  # GenericSolid(ndim, nst, nels, nn, nip, finite_element(nod, nodof), axisymmentrix)
  :element_type => GenericSolid(3, 6, 6, 8, 1, Tetrahedron(4, 3), false),
  :properties => [100.0 0.3 0.0;],
  :g_coord => [
    0.0 0.0 0.0; 1.0 0.0 0.0; 0.0 0.0 -1.0; 1.0 0.0 -1.0;
    0.0 1.0 0.0; 1.0 1.0 0.0; 0.0 1.0 -1.0; 1.0 1.0 -1.0],
  :g_num => [
    1, 3, 4, 7,
    1, 4, 2, 7,
    1, 2, 5, 7,
    6, 4, 8, 7,
    6, 2, 4, 7,
    6, 5, 2, 7
  ],
  :support => [
    (1, [0 0 1]), (2, [1 0 1]), (3, [0 0 0]), (4, [1 0 0]),
    (5, [0 1 1]), (7, [0 1 0]), (8, [1 1 0])
  ],
  :loaded_nodes => [
    ( 1, [0.0 0.0 -0.1667]),
    ( 2, [0.0 0.0 -0.3333]),
    ( 5, [0.0 0.0 -0.3333]),
    ( 6, [0.0 0.0 -0.1667])
  ]
)

@time m = FE5_4(data)
println()
