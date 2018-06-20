using PtFEM, Test, Compat

ProjDir = dirname(@__FILE__)

data = Dict(
  # Plane(ndim, nst, nxe, nye, nip, direction, finite_element(nod, nodof), axisymmetric)
  :struc_el => Plane(2, 4, 40, 20, 4, :y, Quadrilateral(8, 2), false),
  # ϕ (phi, friction angle), c (cohesion), ψ (psi, dilation angle),
  # γ (gamma, soil unit weight), E, ν (v, Poisson's ratio)
  # [ϕ, c, ψ, γ, E, ν]
  :properties => [20.0 10.0 20.0 16.0 1.0e5 0.3;],
  :x_coords => 0.0:0.25:10.0,
  :y_coords => 5.0:-0.25:0.0,
  :tol => 0.001,
  :limit => 200,
  :incs => 10,
  :nbo2 => 8,       # No of elements to be rigidly displaced in bearing capacity 
  :qs => 20,        # Surface surcharge
  :presc => -0.001  # Magnitude of incremental vertical displacements rigid footing
)

@time res_df, g_coord, g_num, disp = p63(data)
println()
res_df

@test res_df[end,:load1] ≈ 274.3514626820244
@test res_df[end,:load2] ≈ 270.01838672066737
@test res_df[end,:iters] == 81
