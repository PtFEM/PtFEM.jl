### Top level structural component ###

@doc doc"""
## StructuralElement

Abstract structural fin_el type.

### Type
```julia
abstract StructuralElement
```

### Subtypes
```julia
* Rod::StructuralElement          : Rod(nxe, np_types, nip, fin_el)
* Beam::StructuralElement         : Beam(nod, nodof)
* Frame::StructuralElement        : Frame(nod, nodof)
* Plane::StructuralElement        : Plane(nod, nodof)
* Solid::StructuralElement        : Solid(nod, nodof)
* GenericSolid::StructuralElement : GenericSolid(nod, nodof)
```
""" ->
abstract StructuralElement              # Structure fin_el to be modeled

@doc doc"""
## Rod

Concrete 1D structural fin_el type with only axial stresses.

### Constructor
```julia
Rod(nels, np_types, nip, fin_el)
```

### Arguments
```julia
* nels::Int64       : Number of fin_els (stored in field nxe)
* np_types::Int64   : Number of different property types
* nip::Int64        : Number of integration points
* fin_el::FiniteElement  : Line(nod, nodof)
```

### Related help
```julia
?StructuralElement  : Help on structural elements
?Element      : Help on finite element types
?Line         : Help on a Line finite element
```

""" ->
immutable Rod <: StructuralElement      # Axial stresses only structural fin_el
  nxe::Int64                      # Number of fin_els
  np_types::Int64                 # Number of property types
  nip::Int64                      # Number of integration points per fin_el
  fin_el::FiniteElement                # Finite fin_el type used
end

type Beam <: StructuralElement          # 1D Beam structural fin_el
  ndim::Int64                     # Number of dimensions (1,2 or 3)
  nst::Int64                      # Number of stress terms
  nxe::Int64                      # Number of fin_els in x direction
  nip::Int64                      # Number of integration points per fin_el
  direction::Symbol               # Node numbering direction
  fin_el::FiniteElement                # Finite fin_el type used
  axisymmetric::Bool              # Axisymmetric
end

type Beam2D <: StructuralElement
  ndim::Int64                     # Number of dimensions (1,2 or 3)
  nst::Int64                      # Number of stress terms
  nxe::Int64                      # Number of fin_els in x direction
  nye::Int64                      # Number of fin_els in y direction
  nip::Int64                      # Number of integration points per fin_el
  direction::Symbol               # Node numbering direction
  fin_el::FiniteElement                # Finite fin_el type used
  axisymmetric::Bool              # Axisymmetric
end

type Beam3D <: StructuralElement
  ndim::Int64                     # Number of dimensions (1,2 or 3)
  nst::Int64                      # Number of stress terms
  nxe::Int64                      # Number of fin_els in x direction
  nye::Int64                      # Number of fin_els in y direction
  nze::Int64                      # Number of fin_els in z direction
  nip::Int64                      # Number of integration points per fin_el
  direction::Symbol               # Node numbering direction
  fin_el::FiniteElement                # Finite fin_el type used
  axisymmetric::Bool              # Axisymmetric
end

# Frame(nels, nn, ndim, finite_element(nod, nodof))
type Frame <: StructuralElement
  nels::Int64                     # Number of elemnt
  nn::Int64                       # Number of nodes
  ndim::Int64                     # Number of dimensions
  nst::Int64                      # Number of stress terms
  nip::Int64                      # Number of integration points per fin_el
  fin_el::FiniteElement                # Finite fin_el type used
end

type Plane <: StructuralElement
  ndim::Int64                     # Number of dimensions (1,2 or 3)
  nst::Int64                      # Number of stress terms
  nxe::Int64                      # Number of fin_els in x direction
  nye::Int64                      # Number of fin_els in y direction
  nip::Int64                      # Number of integration points
  direction::Symbol               # Node numbering direction
  fin_el::FiniteElement                # Finite fin_el type used
  axisymmetric::Bool              # Axisymmetric
end

type Solid <: StructuralElement
  ndim::Int64                     # Number of dimensions (1,2 or 3)
  nst::Int64                      # Number of stress terms
  nxe::Int64                      # Number of fin_els in x direction
  nye::Int64                      # Number of fin_els in y direction
  nze::Int64                      # Number of fin_els in y direction
  nip::Int64                      # Number of integration points
  fin_el::FiniteElement                # Finite fin_el type used
end

type GenericSolid <: StructuralElement
  ndim::Int64                     # Number of dimensions (1,2 or 3)
  nst::Int64                      # Number of stress terms
  nels::Int64                     # Number of fin_els
  nn::Int64                       # Number of nodes
  nip::Int64                      # Number of integration points
  fin_el::FiniteElement                # Finite fin_el type used
  axisymmetric::Bool              # Axisymmetric
end

type UnknownStructuralElement <: StructuralElement
end

