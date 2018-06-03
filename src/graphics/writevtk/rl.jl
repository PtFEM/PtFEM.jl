#!/usr/bin/env julia

ProjDir = dirname(@__FILE__)

# Create rectilinear grid VTK file.

using WriteVTK
import Compat.UTF8String


FloatType = Float32
vtk_filename_noext = "rl"
outfiles = UTF8String[]

dim = 2
x = range(0.0, stop=1.0, length=30)
y = range(0.0, stop=1.0, length=30)

Ni, Nj, Nk = length(x), length(y), 1
z = ones(FloatType, Nk)

# Create some scalar and vectorial data.
p = zeros(FloatType, Ni, Nj, Nk)
q = zeros(FloatType, Ni, Nj, Nk)

for i = 1:Ni, j=1:Nj
  p[i, j, 1] = 10*i
  q[i, j, 1] = 10*i*i
end

cdata = zeros(FloatType, Ni-1, Nj-1)
for j = 1:Nj-1, i = 1:Ni-1
  cdata[i, j] = 2i + 20 * sin(3*pi * (j-1) / (Nj-2))
end

# Test extents (this is optional!!)
ext = [0, Ni-1, 0, Nj-1, 0, Nk-1] + 42

# Initialise new vtr file (rectilinear grid).
vtk = vtk_grid(ProjDir*"/"*vtk_filename_noext*"_$(dim)D", x, y; extent=ext)

# Add data.
vtk_point_data(vtk, p, "p_values")
vtk_point_data(vtk, q, "q_values")
vtk_cell_data(vtk, cdata, "myCellData")

# Save and close vtk file.
append!(outfiles, vtk_save(vtk))

