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

### Top level structural component ###

@doc doc"""
## ElementType

Abstract structural element type.

### Type
```julia
abstract ElementType
```

### Subtypes
```julia
* Rod::ElementType          : Rod(nxe, np_types, nip, element)
* Beam::ElementType         : Beam(nod, nodof)
* Frame::ElementType        : Frame(nod, nodof)
* Plane::ElementType        : Plane(nod, nodof)
* Solid::ElementType        : Solid(nod, nodof)
* GenericSolid::ElementType : GenericSolid(nod, nodof)
```
""" ->
abstract ElementType              # Structure element to be modeled

@doc doc"""
## Rod

Concrete 1D structural element type with only axial stresses.

### Constructor
```julia
Rod(nels, np_types, nip, element)
```

### Arguments
```julia
* nxe::Int64        : Number of elements
* np_types::Int74   : Number of different property types
* nip::Int64        : Number of integration points
* element::Element  : Line(nod, nodof)
```
""" ->
immutable Rod <: ElementType      # Axial stresses only structural element
  nxe::Int64                      # Number of elements
  np_types::Int64                 # Number of property types
  nip::Int64                      # Number of integration points per element
  element::Element                # Finite element type used
end

type Beam <: ElementType          # 1D Beam structural element
  ndim::Int64                     # Number of dimensions (1,2 or 3)
  nst::Int64                      # Number of stress terms
  nxe::Int64                      # Number of elements in x direction
  nip::Int64                      # Number of integration points per element
  direction::Symbol               # Node numbering direction
  element::Element                # Finite element type used
  axisymmetric::Bool              # Axisymmetric
end

type Beam2D <: ElementType
  ndim::Int64                     # Number of dimensions (1,2 or 3)
  nst::Int64                      # Number of stress terms
  nxe::Int64                      # Number of elements in x direction
  nye::Int64                      # Number of elements in y direction
  nip::Int64                      # Number of integration points per element
  direction::Symbol               # Node numbering direction
  element::Element                # Finite element type used
  axisymmetric::Bool              # Axisymmetric
end

type Beam3D <: ElementType
  ndim::Int64                     # Number of dimensions (1,2 or 3)
  nst::Int64                      # Number of stress terms
  nxe::Int64                      # Number of elements in x direction
  nye::Int64                      # Number of elements in y direction
  nze::Int64                      # Number of elements in z direction
  nip::Int64                      # Number of integration points per element
  direction::Symbol               # Node numbering direction
  element::Element                # Finite element type used
  axisymmetric::Bool              # Axisymmetric
end

# Frame(nels, nn, ndim, finite_element(nod, nodof))
type Frame <: ElementType
  nels::Int64                     # Number of elemnt
  nn::Int64                       # Number of nodes
  ndim::Int64                     # Number of dimensions
  nst::Int64                      # Number of stress terms
  nip::Int64                      # Number of integration points per element
  element::Element                # Finite element type used
end

type Plane <: ElementType
  ndim::Int64                     # Number of dimensions (1,2 or 3)
  nst::Int64                      # Number of stress terms
  nxe::Int64                      # Number of elements in x direction
  nye::Int64                      # Number of elements in y direction
  nip::Int64                      # Number of integration points
  direction::Symbol               # Node numbering direction
  element::Element                # Finite element type used
  axisymmetric::Bool              # Axisymmetric
end

type Solid <: ElementType
  ndim::Int64                     # Number of dimensions (1,2 or 3)
  nst::Int64                      # Number of stress terms
  nxe::Int64                      # Number of elements in x direction
  nye::Int64                      # Number of elements in y direction
  nze::Int64                      # Number of elements in y direction
  nip::Int64                      # Number of integration points
  element::Element                # Finite element type used
end

type GenericSolid <: ElementType
  ndim::Int64                     # Number of dimensions (1,2 or 3)
  nst::Int64                      # Number of stress terms
  nels::Int64                     # Number of elements
  nn::Int64                       # Number of nodes
  nip::Int64                      # Number of integration points
  element::Element                # Finite element type used
  axisymmetric::Bool              # Axisymmetric
end

type UnknownElementType <: ElementType
end

