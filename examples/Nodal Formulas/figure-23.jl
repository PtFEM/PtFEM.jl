using PtFEM
using Test

ProjDir = @__DIR__

#=
Compare formulas at:
http://www.awc.org/pdf/codes-standards/publications/design-aids/AWC-DA6-BeamFormulas-0710.pdf
=#

data = Dict(
  # Frame(nels, nn, ndim, nst, nip, finite_element(nod, nodof))
  :struc_el => Frame(20, 21, 3, 1, 1, Line(2, 3)),
  :properties => [1.0e3 1.0e3 1.0e3 3.0e5;],
  :x_coords => range(0, stop=4, length=21),
  :y_coords => zeros(21),
  :z_coords => zeros(21),
  :g_num => [
    collect(1:20)';
    collect(2:21)'],
  :support => [
    (1, [0 0 0 0 0 0]),
    (21, [0 0 0 0 0 0]),
    ],
  :loaded_nodes => [
      ( 1, [0.0 -1.0 0.0 0.0 0.0 0.0]),
      ( 2, [0.0 -1.0 0.0 0.0 0.0 0.0]),
      ( 3, [0.0 -1.0 0.0 0.0 0.0 0.0]),
      ( 4, [0.0 -1.0 0.0 0.0 0.0 0.0]),
      ( 5, [0.0 -1.0 0.0 0.0 0.0 0.0]),
      ( 6, [0.0 -1.0 0.0 0.0 0.0 0.0]),
      ( 7, [0.0 -1.0 0.0 0.0 0.0 0.0]),
      ( 8, [0.0 -1.0 0.0 0.0 0.0 0.0]),
      ( 9, [0.0 -1.0 0.0 0.0 0.0 0.0]),
      (10, [0.0 -1.0 0.0 0.0 0.0 0.0]),
      (11, [0.0 -1.0 0.0 0.0 0.0 0.0]),
      (12, [0.0 -1.0 0.0 0.0 0.0 0.0]),
      (13, [0.0 -1.0 0.0 0.0 0.0 0.0]),
      (14, [0.0 -1.0 0.0 0.0 0.0 0.0]),
      (15, [0.0 -1.0 0.0 0.0 0.0 0.0]),
      (16, [0.0 -1.0 0.0 0.0 0.0 0.0]),
      (17, [0.0 -1.0 0.0 0.0 0.0 0.0]),
      (18, [0.0 -1.0 0.0 0.0 0.0 0.0]),
      (19, [0.0 -1.0 0.0 0.0 0.0 0.0]),
      (20, [0.0 -1.0 0.0 0.0 0.0 0.0]),
      (21, [0.0 -1.0 0.0 0.0 0.0 0.0])
    ]
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

if VERSION.minor == 6
  using Plots
  gr(size=(400,600))

  p = Vector{Plots.Plot{Plots.GRBackend}}(undef, 3)
  p[1] = plot(m.x_coords, m.displacements[2,:], ylim=(-0.004, 0.001), lab="Displacement", 
   xlabel="x [m]", ylabel="deflection [m]", color=:red)
  p[2] = plot(m.actions[2,:], lab="Shear force", ylim=(-10, 10), xlabel="element",
    ylabel="shear force [N]", palette=:greens,fill=(0,:auto),α=0.6)
  p[3] = plot(m.actions[12,:], lab="Moment", ylim=(-7, 7), xlabel="element",
    ylabel="moment [Nm]", palette=:grays,fill=(0,:auto),α=0.6)

  plot(p..., layout=(3, 1))
  savefig(ProjDir*"/figure-23.png")
  #=
  plot!()
  gui()
  =#
end

if VERSION.minor > 5

  # See figure 24 in above reference (Δmax): 
  @eval @test m.displacements[2,11] ≈ -10000 * 4^3 / (192 * 1.0e6) atol=10.0*eps()

  # See figure 24 in above reference (Mmax): 
  @eval @test m.actions[12,10] ≈ (10000 * 4 / 8) atol=10.0*eps()
end
  