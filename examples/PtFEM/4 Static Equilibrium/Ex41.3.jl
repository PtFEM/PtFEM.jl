using PtFEM

ProjDir = dirname(@__FILE__)

# This example is identical to p.4.1.1.jl but re-uses the Cholesky
# decomposed global stiffness matrix in the 2nd part by calling
# p41(m, data) where the loaded_nodes values are doubled. 

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
  :struc_el => getfield(Main, Symbol(struct_el))(els, np_types, nip,
    getfield(Main, Symbol(fin_el))(nod, nodof)),
  :properties => [EA;],
  :x_coords => 0.0:l/els:l,
  :support => [(N, [0])],
  :loaded_nodes => [
      (1, [-0.625]),
      (2, [-1.25]),
      (3, [-1.25]),
      (4, [-1.25]),
      (5, [-0.625])
    ]
);

data |> display
println()

@time fem, dis_df, fm_df = p41(data)
println()

# Update (double) the loaded_nodes values

F = -10.0
data[:loaded_nodes] = [(i, [0.0]) for i in 1:N]
for node in 1:N-1
  data[:loaded_nodes][node][2][1] += F / (2els)
  data[:loaded_nodes][node+1][2][1] += F / (2els)
end  

# Pass in m so m.cfgsm can be re-used with this new load vector.

@time fem2, dis_df2, fm_df2 = p41(fem, data)
println()

display(dis_df2)
println()
display(fm_df2)
