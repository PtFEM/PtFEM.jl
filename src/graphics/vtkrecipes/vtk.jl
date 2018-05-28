#!/usr/bin/env julia

# Create rectilinear grid VTK file.

using DataFrames, WriteVTK
import Compat.UTF8String
const FloatType = Float32

"""
## vtk

Plots displacements and directions

### Function
```julia
mesh(data, fm_df, sigma_df, dir, fname)
```

### Arguments
```julia
* data::Dict                 : Input dictionary
* fm_df::DataFrame           : Forces and moments DataFrame
* sigma_df::DataFrame        : Stresses DataFrame
* dir::AbstractString        : Project directory
* fname::AbstractString      : Output VTK file name
```
"""
function vtk(data::Dict, fm_df, sigma_df, dir, fname)
  vtk_filename_noext = dir*"/"*fname
  dim = data[:struc_el].ndim
  
  # Define grid
  
  Ni, Nj = length(data[:x_coords]), length(data[:y_coords])
  if dim == 2
    Nk = 1
  elseif dim == 3
    Nk = length(data[:z_coords])
  end

  x = data[:x_coords]
  y = data[:y_coords]
  z = zeros(FloatType, Nk)
  if dim == 3
    z = data[:z_coords]
  end
  
  # Extract node data
  
  disp = Vector{FloatType}(convert(Array, fm_df[:disp]))
  rotx = Vector{FloatType}(convert(Array, fm_df[:rotx]))
  roty = Vector{FloatType}(convert(Array, fm_df[:roty]))
  twistxy = Vector{FloatType}(convert(Array, fm_df[:twistxy]))

  # Extract cell data
  # Note that in structured grids, the cells are the hexahedra (3D) or quads (2D)
  # formed between grid points.
  
  local cdata, sigx, sigy, tauxy
  if dim == 2
      sigx = reshape(convert(Array{FloatType}, sigma_df[:sigx]), 2, 2)
      sigy = reshape(convert(Array{FloatType}, sigma_df[:sigy]), 2, 2)
      tauxy = reshape(convert(Array{FloatType}, sigma_df[:tauxy]), 2, 2)
  elseif dim == 3
      cdata = zeros(FloatType, Ni-1, Nj-1, Nk-1)
      for k = 1:Nk-1, j = 1:Nj-1, i = 1:Ni-1
          cdata[i, j, k] = 2i + 3k * sin(3*pi * (j-1) / (Nj-2))
      end
  end

  # Test extents (this is optional!!)
  ext = [0, Ni-1, 0, Nj-1, 0, Nk-1] + 42

  # Initialise new vtr file (rectilinear grid).
  @time begin
      local vtk
      if dim == 2
          vtk = vtk_grid(vtk_filename_noext*"_$(dim)D", x, y; extent=ext)
      elseif dim == 3
          vtk = vtk_grid(vtk_filename_noext*"_$(dim)D", x, y, z;
                         extent=ext)
      end

      # Add data.
      vtk_point_data(vtk, disp, "disp")
      vtk_point_data(vtk, rotx, "rotx")
      vtk_point_data(vtk, roty, "roty")
      vtk_point_data(vtk, twistxy, "twistxy")
      vtk_cell_data(vtk, sigx, "sigx")
      vtk_cell_data(vtk, sigy, "sigy")
      vtk_cell_data(vtk, tauxy, "tauxy")

      # Save and close vtk file.
      vtk_save(vtk)
  end
end
