# poisson_fem.jl
#
# Main file for solving the 2D Poisson equation over the unit square.
# Depends on finite_element.jl
#
# Amuthan Arunkumar Ramabathiran
# (aparyap@gmail.com)
#
# Distributed under The Code Project Open License (CPOL)
# http://www.codeproject.com/info/cpol10.aspx

old = pwd()
ProjDir = Pkg.dir("LHR", "Examples", "Poisson_fem")
cd(ProjDir)

require("finite_element.jl")

# Boundary definition
# returns true if point lies on boundary of unit square
function on_boundary(x, y)
  eps = 1e-12
  is_on_bdy = false
  if (abs(x) <= 0.0 || abs(x-1.0) <= 0.0 ||
      abs(y) <= 0.0 || abs(y-1.0) <= 0.0)
    is_on_bdy = true
  end
  return is_on_bdy
end

# Dirichlet boundary value
dbc(x,y) = 1 + x^2 + 2y^2

# External Load
f_ext(x,y) = -6.0

# Stiffness matrix
function poisson_stiffness(N, dN, i, j)
  dN[i,1]*dN[j,1] + dN[i,2]*dN[j,2]
end

# MAIN
t0 = time()

mesh = UnitSquare(100)
u = fe_space_C0(mesh,dbc)
solve_fe(mesh, u, poisson_stiffness, f_ext)

println("Total time elapsed = ",time() - t0,"s")

write_to_file(mesh,u)
