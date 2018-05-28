using PtFEM

n = 5           # No of nodes
els = n - 1     # No of elements

data = Dict(
  # Frame(nels, nn, ndim, nst, nip, finite_element(nod, nodof))
  :struc_el => Frame(els, n, 3, 1, 1, Line(2, 3)),
  :properties => [
    4.0e6 1.0e6 0.3e6 0.3e6],
  :x_coords => Float64[i for i in 0:n-1],
  :y_coords => zeros(n),
  :z_coords => zeros(n),
  :gamma => zeros(n),     # No rotation of element about local axis (dip)
  :g_num => hcat(1:n-1,2:n)',
  :support => [
    (1, [0 0 0 0 0 0])
  ],
  :loaded_nodes => [
    (n, [0.0 0.0 100.0 0.0 0.0 0.0])
  ]
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

