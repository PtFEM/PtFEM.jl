using CSoM

data = Dict(
  # Beam(ndim, nst, nxe, nip, direction, finite_element(nod, nodof), axisymmet
  :struc_el => Beam(2, 1, 4, 1, :x, Line(2, 2), false),
  :properties => [1.0e4],
  :x_coords => [0.0, 1.0, 2.0, 3.0, 4.0],
  :support => [
    (1, [0 0]),
    (5, [0 0])
    ],
  :loaded_nodes => [
  (1, [-2.0 -4.0/12.0]),
  (2, [-2.0 0.0]),
  (3, [-2.0 0.0]),
  (4, [-2.0 0.0]),
  (5, [-2.0  4.0/12.0])],
  :penalty => 1e19
)

# Compute nodal equivalen moments and forces for elements loaded between nodes
N = data[:struc_el].nxe
k = data[:struc_el].fin_el.nod * data[:struc_el].fin_el.nodof
nodal_forces = zeros(N, k)
nodal_moments = zeros(N, k)
equivalent_nodal_forces = zeros(N, k)
equivalent_nodal_moments = zeros(N, k)

enmv = zeros(N)
enmv[1] = -4.0/12.0
enmv[N] =  4.0/12.0
enfv = [-2.0 for i in 1:N]

lnodes = Array{Tuple{Int64,Array{Float64,2}},1}()
for i in 1:N
  push!(lnodes, (i, [enfv[i] enmv[i]]))
end

data |> display
println()

@time m = FE4_3(data)
println()

if VERSION.minor > 5
  println("Displacements:")
  m.displacements' |> display
  println()

  println("Actions:")
  m.actions' |> display
  println()
else
  using DataFrames
  dis_df = DataFrame(
    translations = m.displacements'[:, 1],
    rotations = m.displacements'[:, 2]
  )
  fm_df = DataFrame(
    xl_Force = m.actions[1, :],
    xl_Moment = m.actions[2, :],
    xr_Force = m.actions[3, :],
    xr_Moment = m.actions[4, :]
  )
  display(dis_df)
  println()
  display(fm_df)
end

