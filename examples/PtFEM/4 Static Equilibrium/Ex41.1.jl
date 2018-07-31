using PtFEM

ProjDir = dirname(@__FILE__)

l = 1.0       # Total length [m]
N = 5         # Number of nodes
els = N - 1   # Number of finite elements (in x direction)
nod = 2       # Number of nodes per finite elements
nodof = 1     # Degrees of freedom for each node
np_types = 1  # Number of property types
EA = 1.0e5    # Strain stiffness
nip = 1       # Number of integration points

data = Dict(
  # StructuralElement(nxe, np_types, nip, FiniteElement(nod, nodof))
  :struc_el => Rod(els, np_types, nip, Line(nod, nodof)),
  :properties => [EA;],
  # Compute x_coords using length l and number of elements, els
  :x_coords => 0.0:l/els:l,
  # Define a support for node N
  # Fix the single dof (x direction displacement) of node N
  :support => [(N, [0])],
  # External forces are applied to nodes 1 to 5.
  :loaded_nodes => [
      (1, [-0.625]),
      (2, [-1.25]),
      (3, [-1.25]),
      (4, [-1.25]),
      (5, [-0.625])
    ]
);

# Display the data dictionary
data |> display
println()

# Solve the FEM model
@time fem, dis_df, fm_df = p41(data)
println()

display(fem.displacements)
println()
display(fem.actions)
println()

display(dis_df)
println()
display(fm_df)
println()
  
if VERSION.minor < 7      # Prevent plotting in Julia v"0.6" for now

  using Plots
  gr(size=(400,500))

  x = 0.0:l/els:l
  u = convert(Array, dis_df[:x_translation])
    
  p = Vector{Plots.Plot{Plots.GRBackend}}(undef, 2)
  titles = ["PtFEM Ex41.1 u(x)", "PtFEM Ex41.1 N(x)"]
   
  p[1]=plot(ylim=(0.0, 1.0), xlim=(0.0, 5.0),
    yflip=true, xflip=false, xlab="Normal force [N]",
    ylab="x [m]", title=titles[2]
  )
  vals = convert(Array, fm_df[:normal_force_2])
  for i in 1:els
      plot!(p[1], 
        [vals[i], vals[i]],
        [(i-1)*l/els, i*l/els], color=:blue,
        fill=true, fillalpha=0.1, leg=false
      )
      delta = abs(((i-1)*l/els) - (i*l/els)) / 20.0
      y1 = collect(((i-1)*l/els):delta:(i*l/els))
      for j in 1:length(y1)
        plot!(p[1],
          [vals[i], 0.0],
          [y1[j], y1[j]], color=:blue, alpha=0.5
        )
      end
  end
  
  p[2] = plot(u, x, xlim=(-0.00003, 0.0), yflip=true,
    xlab="Displacement [m]", ylab="x [m]",
    fillto=0.0, fillalpha=0.1, leg=false, title=titles[1])

  plot(p..., layout=(1, 2))
  savefig(ProjDir*"/Ex41.1.png")
  
end
