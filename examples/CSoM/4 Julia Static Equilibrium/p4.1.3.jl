using CSoM

ProjDir = dirname(@__FILE__)

l = 1.0       # Total length [m]
N = 5         # Number of nodes
els = N - 1   # Number of finite elements
nod = 2       # Number of nodes per finite elements
nodof = 1     # Degrees of freedom for each node
np_types = 1  # Number of proerty types
EA = 1.0e5    # Strain stiffness
nip = 1       # Number of integration points

struct_el = :Rod
fin_el = :Line

data = Dict(
  # Rod(nxe, np_types, nip, fin_el(nod, nodof))
  :struc_el => Rod(els, np_types, nip, Line(nod, nodof)),
  :properties => [EA;],
  :x_coords => 0.0:l/els:l,
);

data[:support] = [(N, [0]);]
data[:loaded_nodes] = [(1, [5.0])]

data |> display
println()

@time m = CSoM.jFE4_1(data)
println()

if VERSION.minor > 5
  println("Displacements:")
  m.displacements |> display
  println()

  println("Actions:")
  m.actions |> display
  println()
else
  using DataTables
  dis_dt = DataTable(
    x_translation = m.displacements[:, 1],
  )
  fm_dt = DataTable(
    normal_force_1 = m.actions[:, 1],
    normal_force_2 = m.actions[:, 2]
  )
  
  display(dis_dt)
  println()
  display(fm_dt)
  println()
end

data[:loaded_nodes] = [(1, [100.0])]

# Note the use of multiple dispatch to select the correct version
# as m is of type jFEM
@time m2 = CSoM.FE4_1(m, data)
println()

if VERSION.minor > 5
  println("Displacements:")
  m2.displacements' |> display
  println()

  println("Actions:")
  m2.actions' |> display
  println()
else
  using DataTables
  dis_dt2 = DataTable(
    x_translation = m2.displacements[:, 1],
  )
  fm_dt2 = DataTable(
    normal_force_1 = m2.actions[:, 1],
    normal_force_2 = m2.actions[:, 2]
  )
    
  display(dis_dt2)
  println()
  display(fm_dt2)
end