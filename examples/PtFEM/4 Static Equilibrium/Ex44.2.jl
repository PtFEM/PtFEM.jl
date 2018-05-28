using PtFEM

data = Dict(
  # Frame(nels, nn, ndim, nst, nip, finite_element(nod, nodof))
  :struc_el => Frame(3, 4, 3, 1, 1, Line(2, 3)),
  :properties => [
    4.0e6 1.0e6 0.3e6 0.3e6],
  :x_coords => [0.0, 5.0, 5.0, 5.0],
  :y_coords => [5.0, 5.0, 5.0, 0.0],
  :z_coords => [5.0, 5.0, 0.0, 0.0],
  :gamma => [0.0, 0.0, 90.0],             # Third element 90 degrees from x-y plane
  :g_num => [
    1 3 4;
    2 2 3],
  :support => [
    (1, [0 0 0 0 0 0]),
    (4, [0 0 0 0 0 0])
    ],
  :loaded_nodes => [
    (2, [0.0 -100.0 0.0 0.0 0.0 0.0])]
)

data |> display
println()

@time fem, dis_df, fm_df = p44(data)
println()

dis_df |> display
println()

println("Actions:")
fm_df |> display
println()

