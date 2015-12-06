### Finite elements used to approximate element ###

@doc doc"""
## Element

Abstract finite element type.

### Type
```julia
abstract Element
```

### Subtypes
```julia
* Line::Element          : Line(nod, nodof)
* Triangle::Element      : Triangle(nod, nodof)
* Quadrilateral::Element : Quadrilateral(nod, nodof)
* Hexahedron::Element    : Hexahedron(nod, nodof)
* Tetrahedron::Element   : Tetrahedron(nod, nodof)
```
""" ->
abstract Element                 # Finite elements

immutable Line <: Element
  nod::Int64
  nodof::Int64
end

immutable Triangle <: Element
  nod::Int64
  nodof::Int64
end

immutable Quadrilateral <: Element
  nod::Int64
  nodof::Int64
end

immutable Hexahedron <: Element
  nod::Int64
  nodof::Int64
end

immutable Tetrahedron <: Element
  nod::Int64
  nodof::Int64
end

immutable UnknownElement <: Element
end
