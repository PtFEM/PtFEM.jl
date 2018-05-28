using PtFEM

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

@time fem, dis_df, fm_df = p43(data)
println()

display(dis_df)
println()
display(fm_df)
