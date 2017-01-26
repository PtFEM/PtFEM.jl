### Finite elements used to approximate fin_el ###

@doc doc"""
## FiniteElement

Abstract finite element type.

### Type
```julia
abstract FiniteElement
```

### Subtypes
```julia
* Line::FiniteElement          : Line(nod, nodof)
* Triangle::FiniteElement      : Triangle(nod, nodof)
* Quadrilateral::FiniteElement : Quadrilateral(nod, nodof)
* Hexahedron::FiniteElement    : Hexahedron(nod, nodof)
* Tetrahedron::FiniteElement   : Tetrahedron(nod, nodof)
```
""" ->
abstract FiniteElement                 # Finite elements

immutable Line <: FiniteElement
  nod::Int64
  nodof::Int64
end

immutable Triangle <: FiniteElement
  nod::Int64
  nodof::Int64
end

immutable Quadrilateral <: FiniteElement
  nod::Int64
  nodof::Int64
end

immutable Hexahedron <: FiniteElement
  nod::Int64
  nodof::Int64
end

immutable Tetrahedron <: FiniteElement
  nod::Int64
  nodof::Int64
end

immutable UnknownFiniteElement <: FiniteElement
end
