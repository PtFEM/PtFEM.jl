# PtFEM.jl Documentation


## Programs

```@docs
p41(data::Dict{Symbol, Any})
p42(data::Dict{Symbol, Any})Line
```

## Structural Elements

```@docs
StructuralElement
Rod
Beam
```

## Finite Elements

```@docs
FiniteElement
Line
Triangle
```

## PtFEM

```@docs
fromSkyline(skyline::Vector{Float64}, kdiag::Vector{Int64})
PtFEM.sparin!(kv::Vector{Float64}, kdiag::Vector{Int64})
```

## Index
```@index
```
