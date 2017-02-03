using CSoM
using Base.Test

#=
Compare formulas at:
http://www.awc.org/pdf/codes-standards/publications/design-aids/AWC-DA6-BeamFormulas-0710.pdf
=#

ProjDir = dirname(@__FILE__)

# Setup loaded nodes entry
function create_figure_1_loaded_nodes_array(x, wl)
  # x        : x coordinate array [m]
  # wl       : load [kN/m]
  
  N = length(x)
  lnodes =  Array{Tuple{Int64,Array{Float64,2}},1}()
  for i in 1:N
    push!(lnodes, (i, [0.0 0.0]))
  end
  
  # update nodes with contribution by fixed loads
  for i in 1:N
    if i == 1
      shearforce = -wl*(x[i+1]-x[i])/2.0      # allocate 1/2 weight to each node
      lnodes[i][2][1] = shearforce
      lnodes[i][2][2] = 0.0
    elseif i == N
      shearforce = -wl*(x[i]-x[i-1])/2.0
      lnodes[i][2][1] = shearforce
      lnodes[i][2][2] = 0.0
    else
      shearforce = -wl*((x[i]-x[i-1]) + (x[i+1]-x[i]))/2.0
      lnodes[i][2][1] = shearforce
      lnodes[i][2][2] = 0.0
    end
  end

  lnodes
end

data = Dict(
  # Beam(ndim, nst, nxe, nip, direction, finite_element(nod, nodof), axisymmet
  :struc_el => Beam(2, 1, 1, 1, :x, Line(2, 2), false),
  :properties => [1.0e4],
  :x_coords => [0.0; 1.0],
  :support => [
    (1, [0 0]),
    (2, [0 0])
    ],
  :loaded_nodes => [
    (1, [-2.0 -4/12]),
    (2, [-2.0  4/12])
    ],
  :penalty => 1e19
)

#data[:loaded_nodes] = create_figure_1_loaded_nodes_array(data[:x_coords], 4.0)
data[:loaded_nodes] |> display
println()

m = FE4_3(data)

println("Displacements:")
m.displacements' |> display
println()

println("Actions:")
m.actions' |> display
println()

if VERSION.minor == 5
  using Plots
  gr(size=(400,600))

  p = Vector{Plots.Plot{Plots.GRBackend}}(3)
  p[1] = plot(m.x_coords, m.displacements[2,:], ylim=(-0.002, 0.003), lab="Displacement", 
   xlabel="x [m]", ylabel="deflection [m]", color=:red)
  p[2] = plot(m.actions'[3,:], lab="Shear force", ylim=(-30, 30), xlabel="element",
    ylabel="shear force [N]", palette=:greens,fill=(0,:auto),α=0.6)
  p[3] = plot(m.actions'[4,:], lab="Moment", ylim=(-2, 2), xlabel="element",
    ylabel="moment [Nm]", palette=:grays,fill=(0,:auto),α=0.6)

  plot(p..., layout=(3, 1))
  savefig(ProjDir*"/figure-23.png")
end
