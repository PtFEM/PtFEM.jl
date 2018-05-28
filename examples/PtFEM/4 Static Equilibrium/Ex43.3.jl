using PtFEM

data = Dict(
  # Beam(ndim, nst, nxe, nip, direction, finite_element(nod, nodof), axisymmet
  :struc_el => Beam(3, 1, 4, 1, :x, Line(2, 3), false),
  :properties => [
    4.0e6 1.0e6 0.3e6 0.3e6],
  :x_coords => [0.0, 1.0, 2.0, 3.0, 4.0],
  :y_coords => zeros(5),
  :z_coords => zeros(5),
  :gamma => [0.0, 0.0, 90.0],
  :g_num => [
    1 2 3 4;
    2 3 4 5],
  :support => [
    (1, [0 0 0])
    ],
  :loaded_nodes => [
    (5, [0.0 -100.0 0.0])]
)

data |> display
println()

@time fem, dis_df, fm_df = p43(data)
println()

display(dis_df)
println()
display(fm_df)
