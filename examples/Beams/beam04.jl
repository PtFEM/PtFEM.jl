using Compat, CSoM

include(Pkg.dir("CSoM", "examples", "StaticEquilibrium", "FE4_4.jl"))


data = @compat Dict(
  # Frame(nels, nn, ndim, nst, nip, finite_element(nod, nodof))
  :element_type => Frame(20, 21, 3, 1, 1, Line(2, 3)),
  :properties => [2.0e6 1.0e6 1.0e6 3.0e5;],
  :x_coords => collect(linspace(0, 4, 21)),
  :y_coords => zeros(21),
  :z_coords => zeros(21),
  :g_num => [
    collect(1:20)';
    collect(2:21)'],
  :support => [
    (1, [0 0 0 0 0 0])
    ],
  :loaded_nodes => [
    (21, [1000.0 0.0 0.0 0.0 0.0 1000.0])],
  :penalty => 1e19
)

data |> display
println()

@time m = FE4_4(data)
println()

println("Displacements:")
m.displacements |> display
println()

println("Actions:")
m.actions |> display
println()

rin = 0.1
rout = 0.11
area = 2pi*(rout^2-rin^2)
@show area

ip = pi/2*(rout^4-rin^4)
@show ip

sigma = m.actions[7,1] / area
@show sigma

sigmamax = m.actions[12,1]*rout/ip
@show sigmamax
println()