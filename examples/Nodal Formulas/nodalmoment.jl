using CSoM

#=
Compare formulas at:
http://www.awc.org/pdf/codes-standards/publications/design-aids/AWC-DA6-BeamFormulas-0710.pdf
=#

data = Dict(
  # Frame(nels, nn, ndim, nst, nip, finite_element(nod, nodof))
  :struc_el => Frame(20, 21, 3, 1, 1, Line(2, 3)),
  :properties => [2.0e6 1.0e6 1.0e6 3.0e5;],
  :x_coords => collect(linspace(0, 4, 21)),
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
    (11, [0.0 -10000.0 0.0 0.0 0.0 0.0])]
)

data |> display
println()

m = FE4_4(data)

println("Displacements:")
m.displacements |> display
println()

println("Actions:")
m.actions |> display
println()

#=
using Plots
plot(m.displacements[2,:])
=#

println("y displacements:")
m.displacements[2,:] |> display
println()

println("y moment actions:")
m.actions[12,:] |> display
println()
