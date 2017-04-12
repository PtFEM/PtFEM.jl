using PtFEM

data = Dict(
  # Frame(nels, nn, ndim, nst, nip, finite_element(nod, nodof))
  :struc_el => Frame(4, 5, 3, 1, 1, Line(2, 2)),
  :properties => [5.0e5;],
  :x_coords => [0.0, 1.25, 3.5, 4.0, 2.0],
  :y_coords => [0.0, 3.0, 2.0, 1.0, 1.5],
  :z_coords => [0.0, 0.0, 0.0, 0.0, 3.0],
  :g_num => [1 2 3 4;5 5 5 5],
  :support => [
    (1, [0 0 0]),
    (2, [0 0 0]),
    (3, [0 0 0]),
    (4, [0 0 0])
    ],
  :loaded_nodes => [
    (5, [20.0 -20.0 30.0])],
  :fixed_freedoms => [
    (5, 2, -0.0005)],
  :penalty => 1e19
)

data |> display
println()

@time m = p42(data)
println()

using DataTables
dis_dt = DataTable(
  x_translation = m.displacements[1, :],
  y_translation = m.displacements[2, :],
  z_translation = m.displacements[3, :],
)
fm_dt = DataTable(
  x_force_1 = m.actions[1, :],
  y_force_1 = m.actions[2, :],
  z_force_1 = m.actions[3, :],
  x_force_2 = m.actions[4, :],
  y_force_2 = m.actions[5, :],
  z_force_2 = m.actions[6, :],
  axial_force = m.axial
)
  
display(dis_dt)
println()
display(fm_dt)
