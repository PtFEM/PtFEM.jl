# Programs

## 4 Static Equilibrium Programs

```@docs
p41
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

## Other Julia Types

```@docs
FEM
jFEM
```

## PtFEM - Main

```@docs
PtFEM.beam_gm!(gm::Matrix{Float64}, ell::Float64)
PtFEM.beam_km!(km::Matrix{Float64}, ei::Float64, ell::Float64)
PtFEM.beam_mm!(mm::Matrix{Float64}, fs::Float64, ell::Float64)
PtFEM.beemat!(bee::Matrix{Float64},deriv::Matrix{Float64})
PtFEM.bmat_nonaxi!(bee::Matrix{Float64}, radius::Float64, coord::Matrix{Float64}, deriv::Matrix{Float64}, fun::Vector{Float64}, iflag::Int64, lth::Int64)
PtFEM.checon!(loads::Vector{Float64}, oldlds::Vector{Float64}, tol::Float64)
PtFEM.deemat!(dee::Array{Float64, 2}, e::Float64, v::Float64)
PtFEM.fkdiag!(kdiag::Vector{Int64}, g::Vector{Int64})
PtFEM.fmplat!(d2x::Vector{Float64}, d2y::Vector{Float64}, d2xy::Vector{Float64}, points::Matrix{Float64}, aa::Float64, bb::Float64, i::Int64)
PtFEM.formm!(stress::Vector{Float64}, m1::Matrix{Float64}, m2::Matrix{Float64}, m3::Matrix{Float64})
PtFEM.formnf!(nodof::Int64, nn::Int64, nf::Matrix{Int64})
PtFEM.fsparm!(gsm, g, km)
PtFEM.glob_to_loc!(loc::Vector{Float64}, glob::Vector{Float64}, gamma::Float64, coord::Matrix{Float64})
PtFEM.glob_to_axial(glob::Vector{Float64}, coord::Matrix{Float64})
PtFEM.hinge!(coord::Matrix{Float64}, holdr::Matrix{Float64}, action::Vector{Float64}, react::Vector{Float64}, prop::Matrix{Float64}, iel, etype::Vector{Int64}, gamma::Vector{Float64})
PtFEM.invar!(stress::Vector{Float64}, sigm::Float64, dsbar::Float64, theta::Float64)
PtFEM.linmul_sky!(kv::Vector{Float64}, disps::Vector{Float64}, loads::Vector{Float64}, kdiag::Vector{Int64})
PtFEM.loc_to_glob!(loc::Vector{Float64}, glob::Vector{Float64}, gamma::Float64, coord::Matrix{Float64})
PtFEM.num_to_g!(num::Vector{Int64}, nf::Matrix{Int64}, g::Vector{Int64})
PtFEM.pin_jointed!(km::Matrix{Float64}, ea::Float64, coord::Matrix{Float64})
PtFEM.rigid_jointed!(km::Matrix{Float64}, prop::Matrix{Float64}, gamma::Vector{Float64}, etype::Vector{Int64}, iel::Int64, coord::Matrix{Float64})
PtFEM.rod_km!(km::Matrix{Float64}, ea::Float64, length::Float64)
PtFEM.rod_mm!(mm::Matrix{Float64}, length::Float64)
PtFEM.sample!
PtFEM.shape_der!(der::Matrix{Float64}, points::Matrix{Float64}, i::Int64)
PtFEM.shape_fun!(fun::Vector{Float64}, points::Matrix{Float64}, i::Int64)
PtFEM.stability(gsm::SparseMatrixCSC{Float64,Int64}, ggm::SparseMatrixCSC{Float64,Int64}, tol::Float64, limit::Int64)
```

## PtFEM - Geom

```@docs
PtFEM.geom_rect!
PtFEM.hexahedron_xz!
PtFEM.mesh_size
```

## PtFEM - Plot methods

```@docs
PtFEM.mesh(data::Dict, g_coord::Array{Float64,2}, g_num::Array{Int, 2}, disp, ampl, pdir)
```

## PtFEM - VTK methods

```@docs
PtFEM.vtk(data::Dict, fm_dt, sigma_dt, dir, fname)
```

## PtFEM - Julia functions & operators

```@docs
cholfact
\
```

## PtFEM - Parallel processing

```@docs
pmap
```

## PtFEM - Deprecated

```@docs
PtFEM.fkdiag!
PtFEM.fromSkyline(skyline::Vector{Float64}, kdiag::Vector{Int64})
PtFEM.fsparv!(kv::Vector{Float64}, km::Matrix{Float64}, g::Vector{Int64}, kdiag::Vector{Int64})
PtFEM.linmul_sky!
PtFEM.skyline2sparse(skyline::Vector{Float64}, kdiag::Vector{Int64})
PtFEM.spabac!(kv::Vector{Float64}, loads::Vector{Float64}, kdiag::Vector{Int64})
PtFEM.sparin!(kv::Vector{Float64}, kdiag::Vector{Int64})
```

## Index
```@index
```
