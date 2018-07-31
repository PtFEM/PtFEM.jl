using PtFEM

ProjDir = dirname(@__FILE__)

data = Dict(
  # Beam(ndim, nst, nxe, nip, direction, finite_element(nod, nodof), axisymmetric)
  :struc_el => Beam(2, 1, 100, 1, :x, Line(2, 1), false),
  :properties => [1.0;],
  :x_coords => 0.0:0.01:1.0,
  :support => [
    (1, [0 1]),
    (101, [0 0])
    ],
  :limit => 100,
  :tol => 0.00001
)

data |> display
println()

@time m = p46(data)
println()

println("\nThe buckling load = $(m[1])\n")
buckling_df = DataFrame(
  translation = m[4][m[5][1,:]+1],
  rotation = m[4][m[5][2,:]+1]
)
display(buckling_df)
println()
  
if VERSION.minor < 6
  using Plots
  gr(size=(400,600))

  p = Vector{Plots.Plot{Plots.GRBackend}}(undef, 2)
  titles = ["p46.1a translation", "p46.1a rotation"]
  p[1] = plot(
    convert(Array, buckling_df[:translation]), 
    ylim=(-0.1, 0.3), xlabel="node",
    ylabel="y translation [m]", color=:blue,
    marker=(:circle,1,0.1,stroke(1,:black)),
    title=titles[1], leg=false)
  p[2] = plot(
    convert(Array, buckling_df[:rotation]),
    ylim=(-1.0, 1.0), xlabel="node", 
    ylabel="rotation [radians]", color=:red,
    marker=(:circle,1,0.1,stroke(1,:black)),
    title=titles[2], leg=false)

  plot(p..., layout=(2, 1))
  savefig(ProjDir*"/Ex46.1a.png")
  
end
