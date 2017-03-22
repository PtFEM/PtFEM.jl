using CSoM

data = Dict(
  # Frame(nels, nn, ndim, np_types, nip, finite_element(nod, nodof))
  :struc_el => Frame(10, 6, 2, 1, 1, Line(2, 2)),
  :properties => [2.0e5;],
  :x_coords => [0.0, 4.0, 4.0, 8.0, 8.0, 12.0],
  :y_coords => [3.0, 0.0, 3.0, 3.0, 0.0, 0.0],
  :g_num => [
    1 1 3 3 3 2 2 5 4 5;
    2 3 4 5 2 4 5 4 6 6
  ],
  :support => [
    (1, [0 0]),
    (2, [1 0])
    ],
  :loaded_nodes => [
    (6, [0.0 -10.0])],
  :penalty => 1e19
)

data |> display
println()

@time m = p4_2(data)
println()

using DataTables
dis_dt = DataTable(
  x_translation = m.displacements[1, :],
  y_translation = m.displacements[2, :],
)
fm_dt = DataTable(
  x_force_1 = m.actions[1, :],
  y_force_1 = m.actions[2, :],
  x_force_2 = m.actions[3, :],
  y_force_2 = m.actions[4, :],
  axial_force = m.axial
)
  
display(dis_dt)
println()
display(fm_dt)