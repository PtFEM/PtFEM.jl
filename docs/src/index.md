# Programs

## 4 Static Equilibrium Programs

```@docs
p41(data::Dict{Symbol, Any})
p42(data::Dict{Symbol, Any})
p43(data::Dict{Symbol, Any})
p44(data::Dict{Symbol, Any})
p45(data::Dict{Symbol, Any})
p46(data::Dict{Symbol, Any})
p47(data::Dict{Symbol, Any})
```

## 5 Elastic Solids Programs

```@docs
p51(data::Dict{Symbol, Any})
p52(data::Dict{Symbol, Any})
p53(data::Dict{Symbol, Any})
p54(data::Dict{Symbol, Any})
p55(data::Dict{Symbol, Any})
p56(data::Dict{Symbol, Any})
```

## 6 Material Nonlinearity Programs

```@docs
p61(data::Dict{Symbol, Any})
p62(data::Dict{Symbol, Any})
```

## Structural Element Types

```@docs
StructuralElement
Rod
Beam
Frame
Plane
Solid
GenericSolid
```

## Finite Element Types

```@docs
FiniteElement
Line
Triangle
Quadrilateral
Hexahedron
Tetrahedron
```

## PtFEM - Main

```@docs
PtFEM.beam_gm!(gm::Matrix{Float64}, ell::Float64)
PtFEM.beam_km!(km::Matrix{Float64}, ei::Float64, ell::Float64)
PtFEM.beam_mm!(mm::Matrix{Float64}, fs::Float64, ell::Float64)
PtFEM.beemat_nonaxi!(bee::Matrix{Float64}, radius::Float64, coord::Matrix{Float64}, deriv::Matrix{Float64}, fun::Vector{Float64}, iflag::Int64, lth::Int64)
PtFEM.beemat!(bee::Matrix{Float64},deriv::Matrix{Float64})
PtFEM.checon!(loads::Vector{Float64}, oldlds::Vector{Float64}, tol::Float64)
PtFEM.deemat!(dee::Array{Float64, 2}, e::Float64, v::Float64)
PtFEM.fkdiag!(ndof::Int64, neq::Int64, g::Vector{Int64}, kdiag::Vector{Int64})
PtFEM.fkdiag!(kdiag::Vector{Int64}, g::Vector{Int64})
PtFEM.fmplat!(d2x::Vector{Float64}, d2y::Vector{Float64}, d2xy::Vector{Float64}, points::Matrix{Float64}, aa::Float64, bb::Float64, i::Int64)
PtFEM.formm!(stress::Vector{Float64}, m1::Matrix{Float64}, m2::Matrix{Float64}, m3::Matrix{Float64})
PtFEM.formnf!(nodof::Int64, nn::Int64, nf::Matrix{Int64})
PtFEM.glob_to_loc!(loc::Vector{Float64}, glob::Vector{Float64}, gamma::Float64, coord::Matrix{Float64})
PtFEM.global_to_axial(glob::Vector{Float64}, coord::Matrix{Float64})
```

## PtFEM - Geom

```@docs
PtFEM.geom_rect!(fin_el::Quadrilateral, iel::Int64, x_coords::Array{Float64, 1}, y_coords::Array{Float64, 1}, coord::Matrix{Float64}, num::Vector{Int64}, dir::Symbol)
PtFEM.geom_rect!(fin_el::Triangle, iel::Int64, x_coords::Array{Float64, 1}, y_coords::Array{Float64, 1}, coord::Matrix{Float64}, num::Vector{Int64}, dir::Symbol)
hexahedron_xz!(iel::Int64, x_coords::Vector{Float64}, y_coords::Vector{Float64}, z_coords::Vector{Float64}, coord::Matrix{Float64}, num::Vector{Int64})
PtFEM.mesh_size(fe::Line, nxe::Int64)
PtFEM.mesh_size(fe::Triangle, nxe::Int64, nye::Int64)
PtFEM.mesh_size(fe::Quadrilateral, nxe::Int64, nye::Int64)
PtFEM.mesh_size(fe::Hexahedron, nxe::Int64, nye::Int64, nze::Int64)
```

## PtFEM - Plot methods

```@docs
PtFEM.mesh(data::Dict, g_coord::Array{Float64,2}, g_num::Array{Int, 2}, disp, ampl, pdir)
```

## PtFEM - VTK methods

```@docs
PtFEM.vtk(data::Dict, fm_dt, sigma_dt, dir, fname)
```

## PtFEM - Parallel processing

```@docs
```

## PtFEM - Other

```@docs
fromSkyline(skyline::Vector{Float64}, kdiag::Vector{Int64})
```

## PtFEM - No longer used

```@docs
PtFEM.sparin!(kv::Vector{Float64}, kdiag::Vector{Int64})
```

## Index
```@index
```
