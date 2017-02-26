using CSoM

ProjDir = dirname(@__FILE__)

l = 1.0       # Total length [m]
q = 5.0       # Distributed load [N/m]
N = 20         # Number of nodes
els = N - 1   # Number of elements

data = Dict(
  # Rod(nels, np_types, nip, Line(nod, nodof))
  :struc_el => Rod(N-1, 1, 1, Line(2, 1)),
  :properties => [1.0e5;],
  :x_coords => 0.0:l/els:l,
  :support => [(N, [0])],
)

data[:loaded_nodes] = [(i, [0.0]) for i in 1:N]
for node in 1:N-1
  data[:loaded_nodes][node][2][1] += q*l / (2els)
  data[:loaded_nodes][node+1][2][1] += q*l / (2els)
end  

data[:eq_nodal_forces_and_moments] = [(i, [0.0 0.0]) for i in 1:els]
for node in 1:els
  data[:eq_nodal_forces_and_moments][node][2][1] = q*l / (2els)
  data[:eq_nodal_forces_and_moments][node][2][2] = q*l / (2els)
end  

data |> display
println()

@time m = FE4_1(data)
println()

# Stiffness matrix
println("Stiffness matrix:")
fromSkyline(m.kv, m.kdiag) |> display
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
    x_translation = m.displacements[:, 1],
  )
  fm_df = DataFrame(
    normal_force_1 = m.actions[:, 1],
    normal_force_2 = m.actions[:, 2],
  )
  # Correct element forces and moments for equivalent nodal
  # forces and moments introduced for loading between nodes
  if :eq_nodal_forces_and_moments in keys(data)
    eqfm = data[:eq_nodal_forces_and_moments]
    k = data[:struc_el].fin_el.nod * data[:struc_el].fin_el.nodof
    for t in eqfm
      for i in 1:k
        fm_df[t[1], i] -= round(t[2][i], 2)
      end
    end
  end
    
  display(dis_df)
  println()
  display(fm_df)
  
  using Plots
  gr(size=(400,600))

  p = Vector{Plots.Plot{Plots.GRBackend}}(2)
  titles = ["EEM fig 1.1 u(x)", "EEM fig 1.1 N(x)"]
  fors = vcat(fm_df[:, :normal_force_1], q*l)
  
  p[2] = plot(data[:x_coords], m.displacements[:, 1],
    xlim=(0, l), ylim=(0.0, 0.00003),
    xlabel="x [m]", ylabel="Deflection [m]", color=:red,
    line=(:dash,1), marker=(:circle,1,0.8,stroke(1,:black)),
    title=titles[1], leg=false)
  p[1] = plot(data[:x_coords], -fors,
    xlim=(0,l), ylim=(-6.0, 0.0),
    xlabel="x [m]", ylabel="Normal force [N]", color=:blue,
    line=(:dash,1), marker=(:dot,1,0.8,stroke(1,:black)),
    title=titles[2], leg=false)

  plot(p..., layout=(2, 1))
  savefig(ProjDir*"/EEM_fig1.1.png")
  
end
