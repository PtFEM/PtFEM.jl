### Finite elements used to approximate element ###

abstract Element                 # Finite elements

type Line <: Element
  nod::Int64
  nodof::Int64
end

type Triangle <: Element
  nod::Int64
  nodof::Int64
end

type Quadrilateral <: Element
  nod::Int64
  nodof::Int64
end

type Hexahedron <: Element
  nod::Int64
  nodof::Int64
end

type Tetrahedron <: Element
  nod::Int64
  nodof::Int64
end

### Top level component ###

abstract ElementType              # Structure element to be modeled

type Rod <: ElementType
  ndim::Int64                     # Number of dimensions (1,2 or 3)
  nst::Int64                      # Number of stress terms
  nxe::Int64                      # Number of elements in x direction
  nip::Int64                      # Number of integration points per element
  direction::Symbol               # Node numbering direction
  element::Element                # Finite element type used
  axisymmetric::Bool              # Axisymmetric
end

type Beam <: ElementType
  ndim::Int64                     # Number of dimensions (1,2 or 3)
  nst::Int64                      # Number of stresses
  nxe::Int64                      # Number of elements in x direction
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
  nst::Int64                      # Number of stresses
  nip::Int64                      # Number of integration points per element
  element::Element                # Finite element type used
end

type Plane <: ElementType
  ndim::Int64                     # Number of dimensions (1,2 or 3)
  nst::Int64                      # Number of stresses
  nxe::Int64                      # Number of elements in x direction
  nye::Int64                      # Number of elements in y direction
  nip::Int64                      # Number of integration points
  direction::Symbol               # Node numbering direction
  element::Element                # Finite element type used
  axisymmetric::Bool              # Axisymmetric
end

type Solid <: ElementType
  ndim::Int64                     # Number of dimensions (1,2 or 3)
  nst::Int64                      # Number of stresses
  nxe::Int64                      # Number of elements in x direction
  nye::Int64                      # Number of elements in y direction
  nze::Int64                      # Number of elements in y direction
  nip::Int64                      # Number of integration points
  element::Element                # Finite element type used
end

type GenericSolid <: ElementType
  ndim::Int64                     # Number of dimensions (1,2 or 3)
  nst::Int64                      # Number of stresses
  nels::Int64                     # Number of elements
  nn::Int64                       # Number of nodes
  nip::Int64                      # Number of integration points
  element::Element                # Finite element type used
  axisymmetric::Bool              # Axisymmetric
end

