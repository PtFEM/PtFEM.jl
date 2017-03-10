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
  :support => [(N, [0])],
  :loaded_nodes => [(1, [5.0])]
);

@time m = FE4_1(data)
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
    x_translation = m.displacements[:, 1],
  )
  fm_dt = DataTable(
    normal_force_1 = m.actions[:, 1],
    normal_force_2 = m.actions[:, 2]
  )
    
  display(dis_dt)
  println()
  display(fm_dt)
  
end
