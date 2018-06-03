using PtFEM, Test

data = Dict(
  # Beam(nels, nn, ndim, nst, nip, finite_element(nod, nodof))
  :struc_el => Beam(2, 1, 4, 1, :x, Line(2, 2), false),
  :properties => [4.0e4; 2.0e4],
  :etype => [1, 1, 2, 2],
  :x_coords => [0.0, 2.5, 5.0, 8.0, 10.0],
  :loaded_nodes => [
  (2, [-20.0 0.0]),
  (3, [-6.0 -3.0]),
  (4, [-8.8 2.2]),
  (5, [-1.2 0.5333])],
  :fixed_freedoms => [
    (1, 1, 0.0),
    (1, 2, -0.001),
    (3, 1, -0.005),
    (4, 1, 0.0)],
  :penalty => 1e19
)

@time fem, dis_df, fm_df = p43(data)

@test round.(fem.displacements[:, 5], digits=7) == [0.0047132, 0.0023433]
