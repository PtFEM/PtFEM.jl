using Base.Test, PtFEM, Compat

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
  :incs => 25,
  :nbo2 => 8,       # No of elements to be rigidly displaced in bearing capacity 
  :qs => 20,        # Surface surcharge
  :presc => -0.001  # Magnitude of incremental vertical displacements rigid footing
)

if VERSION.minor > 5 
  @time res_dt, g_coord, g_num, disp = p63(data)
else
  data[:incs] = 5
  @time res_dt, g_coord, g_num, disp = p63_skyline(data)
end
println()
res_dt

if VERSION.minor > 5 
  @test Float64(res_dt[end,:load1].value) ≈ 379.91559664874114
  @test Float64(res_dt[end,:load2].value) ≈ 374.0927319888041
  @test Int(res_dt[end,:iters].value) == 200
else
  @test Float64(res_dt[5,:load1].value) ≈ 161.274
  @test Float64(res_dt[5,:load2].value) ≈ 158.059
  @test Int(res_dt[5,:iters].value) == 30
end  