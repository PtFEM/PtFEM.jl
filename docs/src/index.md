# Programs

## 4 Static Equilibrium Programs

```@docs
p41(data::Dict{Symbol, Any})
p42(data::Dict{Symbol, Any})
p43(data::Dict{Symbol, Any})
```

## 5 Elastic Solids Programs

```@docs
p51(data::Dict{Symbol, Any})
p52(data::Dict{Symbol, Any})
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
PtFEM.formnf!(nodof::Int64, nn::Int64, nf::Matrix{Int64})
```

## PtFEM - Geom

```@docs
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
