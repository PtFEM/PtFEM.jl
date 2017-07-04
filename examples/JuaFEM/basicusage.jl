using JuAFEM

# Create a quadrature rule
quad_rule = QuadratureRule{2, RefCube}(:legendre, 2)

# Create a function space
func_space = Lagrange{2, RefCube, 1}()

# Use these to create an FEValues object
cell_values = CellScalarValues(quad_rule, func_space)
fieldnames(cell_values) |> display
println()

# Coordinates of 1 cell in grid
x = Vec{2, Float64}[
  Vec{2}((0.0, 0.0)),
  Vec{2}((1.5, 0.0)),
  Vec{2}((2.0, 2.0)),
  Vec{2}((0.0, 1.0))
];

# Use reinit! to update cell_values on an element
reinit!(cell_values, x)

# Query cell_values shape function in quadrature point 3
shape_value(cell_values, 3, 1) |> display
println()

# DErivative of shape function 2 in quadrature point 1
shape_gradient(cell_values, 1, 2) |> display
println()

# We can also evaluate on the finite element basis
T = [0.0, 1.0, 2.0, 1.5]; # nodal values
function_value(cell_values, 3, T) |> display # value of T in 3rd quad point
println()
function_gradient(cell_values, 1, T) |> display # value of grad(T) in 1st quad point
println()

# Or for vector valued functions
u = Vec{2, Float64}[
  Vec{2}((0.0, 0.0)),
  Vec{2}((3.5, 2.0)),
  Vec{2}((2.0, 2.0)),
  Vec{2}((2.0, 1.0))
]; # nodal vectors

function_value(cell_values, 2, u) |> display # value of u in 2nd quad point
println()
function_symmetric_gradient(cell_values, 3, u)  |> display# sym(grad(u)) in 3rd quad point