using Plots
gr(size=(400,400))

"""
## mesh

Plots displacements and directions

### Function
```julia
mesh(data, g_coord, g_num, disp, ampl, pdir)
```

### Arguments
```julia
* data::Dict                 : Input dictionary
* g_coord::Array{Float64, 2} : Coordinate array
* g_num::Array{Int, 2}       : Global node numbering array
* disp::DataFrame            : Displacements DataFrame
* ampl::Float64              : Amplification for derivatives
* pdir::AbstractString       : Nodal freedom matrix (updated)
```
"""
function mesh(data::Dict, g_coord::Array{Float64,2}, g_num::Array{Int, 2},
    disp, ampl, pdir)
  plot(leg=false, title="Nodes (blue), $(ampl)*displacements (red)")
  for xc in data[:x_coords]
    for yc in data[:y_coords]
      plot!([xc, xc], [minimum(data[:y_coords]), maximum(data[:y_coords])],
        line=(:solid), color=:lightgray)
      plot!([minimum(data[:x_coords]), maximum(data[:x_coords])],
        [yc, yc], line=(:solid), color=:lightgray)
    end
  end
  vx = ampl*convert(Array, disp[:,1])
  vy = ampl*convert(Array, disp[:,2])
  scatter!(g_coord[1,:], g_coord[2,:], marker=(:circle,2,0.4,stroke(1,:blue)))
  quiver!(g_coord[1,:], g_coord[2,:], quiver=(vx, vy))
  savefig(pdir*"/Ex61.1.jl_dis.png")
  plot!()
end