using PtFEM
using Test

ProjDir = dirname(@__FILE__)

#=
Compare formulas at:
http://www.awc.org/pdf/codes-standards/publications/design-aids/AWC-DA6-BeamFormulas-0710.pdf
=#

data = Dict(
  # Frame(nels, nn, ndim, nst, nip, finite_element(nod, nodof))
  :struc_el => Frame(200, 201, 3, 1, 1, Line(2, 3)),
  :properties => [1.0e6 1.0e6 1.0e6 3.0e5;],
  :x_coords => range(0, stop=4, length=201),
  :y_coords => zeros(201),
  :z_coords => zeros(201),
  :g_num => [
    collect(1:200)';
    collect(2:201)'],
  :support => [
    (1, [0 0 0 0 0 0]),
    (201, [0 0 0 0 0 0]),
    ],
  :loaded_nodes => [
    (101, [0.0 -10000.0 0.0 0.0 0.0 0.0])]
)

data |> display
println()

m, dis_df, fm_df = p44(data)

println("Displacements:")
m.displacements |> display
println()

println("Actions:")
m.actions |> display
println()

println("y displacements:")
m.displacements[2,:] |> display
println()

println("y moment actions:")
m.actions[12,:] |> display
println()
if VERSION.minor == 5
  using Plots
  gr(size=(400,600))

  p = Vector{Plots.Plot{Plots.GRBackend}}(undef, 3)
  p[1] = plot(m.x_coords, m.displacements[2,:], ylim=(-0.004, 0.001), lab="Displacement", 
   xlabel="x [m]", ylabel="deflection [m]", color=:red)
  p[2] = plot(m.actions[2,:], lab="Shear force", ylim=(-6000, 6000), xlabel="element",
    ylabel="shear force [N]", palette=:greens,fill=(0,:auto),α=0.6)
  p[3] = plot(m.actions[12,:], lab="Moment", ylim=(-6000, 6000), xlabel="element",
    ylabel="moment [Nm]", palette=:grays,fill=(0,:auto),α=0.6)

  plot(p..., layout=(3, 1))
  savefig(ProjDir*"/figure-24.png")
  #=
  plot!()
  gui()
  =#
end

if VERSION.minor > 5

  # See figure 24 in above reference (Δmax): 
  @eval @test round.(m.displacements[2,101] digits=11) ≈ 0.00333333333

  # See figure 24 in above reference (Mmax): 
  @eval @test m.actions[12,101] ≈ 4899.999997801155
end
  