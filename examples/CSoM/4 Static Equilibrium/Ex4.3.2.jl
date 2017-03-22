using CSoM

data = Dict(
  # Beam(ndim, nst, nxe, nip, finite_element(nod, nodof), axisymmetric)
  :struc_el => Beam(2, 1, 5, 1, :x, Line(2, 2), false),
  :properties => [
    1.924e4 0.2;
    1.924e4 0.6;
    1.924e4 1.0;
    1.924e4 1.4;
    1.924e4 1.8],
  :etype => [1, 2, 3, 4, 5],
  :x_coords => [0.0, 2.0, 4.0, 6.0, 8.0, 10.0],
  :loaded_nodes => [(1, [1.0 0.0])]
)

data |> display
println()

@time m = p4_3(data)
println()

if VERSION.minor > 5
  println("Displacements:")
  m.displacements' |> display
  println()

  println("Actions:")
  m.actions' |> display
  println()
else
  using DataTables
  dis_dt = DataTable(
    translations = m.displacements[1, :],
    rotations = m.displacements[2, :]
  )
  fm_dt = DataTable(
    xl_Force = m.actions[1, :],
    xl_Moment = m.actions[2, :],
    xr_Force = m.actions[3, :],
    xr_Moment = m.actions[4, :]
  )
  # Correct element forces and moments for equivalent nodal
  # forces and moments introduced for loading between nodes
  if :eq_nodal_forces_and_moments in keys(data)
    eqfm = data[:eq_nodal_forces_and_moments]
    k = data[:struc_el].fin_el.nod * data[:struc_el].fin_el.nodof
    for t in eqfm
      vals = convert(Array, fm_dt[t[1], :])
      for i in 1:k
        fm_dt[t[1], i] = round(vals[i] - t[2][i], 2)
      end
    end
  end
    
  display(dis_dt)
  println()
  display(fm_dt)
end