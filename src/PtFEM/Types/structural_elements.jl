### Top level structural component ###

"""
## StructuralElement

Abstract structural element type.

### Type
```julia
abstract type StructuralElement end
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

### Related help
```julia
?FiniteElement                    : Show all finite elements
?Rod                              : Help on Rod structural element
?Beam                             : Help on Beam structural element
?Frame                            : Help on Frame structural element
?Plane                            : Help on Plane structural element
?Solid                            : Help on Solid structural element
?GenericSolid                     : Help on GenericSolid structural element
```
"""
abstract type StructuralElement end

"""
## Rod

Concrete 1D structural element with only axial stresses.

### Constructor
```julia
Rod(nels, np_types, nip, fin_el)
```

### Arguments
```julia
* nels::Int             : Number of fin_els (stored in field nxe)
* np_types::Int         : Number of different property types
* nip::Int              : Number of integration points
* fin_el::FiniteElement : Line(nod, nodof)
```

### Related help
```julia
?StructuralElement      : Help on structural elements
?FiniteElement          : Help on finite element types
?Line                   : Help on a Line finite element
```
"""
mutable struct Rod <: StructuralElement  # Axial stresses only structural element
  nxe::Int                        # Number of fin_els
  np_types::Int                   # Number of property types
  nip::Int                        # Number of integration points per fin_el
  fin_el::FiniteElement           # Finite fin_el type used (must be Line(...))
end

"""
## Beam

Concrete structural element with transverse and moment loading.

### Constructor
```julia
Beam(ndim, nst, nxe, nip, direction, fin_el, axisymmetric)
```

### Arguments
```julia
* ndim::Int             : Number of dimensions
* nst::Int              : Number of stress terms
* nxe::Int              : Number of different property types
* nip::Int              : Number of integration points
* direction::Symbol     : Number of integration points
* fin_el::FiniteElement : Line(nod, nodof)
* axisymmetric::Bool    : Axisymmetric if true
```

### Related help
```julia
?StructuralElement      : Help on structural elements
?FiniteElement          : Help on finite element types
?Line                   : Help on a Line finite element
```
"""
mutable struct Beam <: StructuralElement    # 1D Beam structural element
  ndim::Int                       # Number of dimensions (1,2 or 3)
  nst::Int                        # Number of stress terms
  nxe::Int                        # Number of fin_els in x direction
  nip::Int                        # Number of integration points per fin_el
  direction::Symbol               # Node numbering direction
  fin_el::FiniteElement           # Finite fin_el type used
  axisymmetric::Bool              # Axisymmetric
end

"""
## Frame

Pin- or rigid-jointed structural element.

### Constructor
```julia
Frame(nels, nn, ndim, finite_element(nod, nodof))
```

### Arguments
```julia
* nels::Int             : Number of elements
* nn:Int                : Number of nodes
* ndim::Int             : Number of dimensions
* nst::Int              : Number of stress terms
* nip::Int              : Number of integration points
* fin_el::FiniteElement : Line(nod, nodof)
```

### Related help
```julia
?StructuralElement  : List structural elements
?FiniteElement      : List finite element types
?Line               : Help on a Line finite element
```
"""
mutable struct Frame <: StructuralElement
  nels::Int                     # Number of elements
  nn::Int                       # Number of nodes
  ndim::Int                     # Number of dimensions
  nst::Int                      # Number of stress terms
  nip::Int                      # Number of integration points per fin_el
  fin_el::FiniteElement         # Finite fin_el type used
end

"""
## Plane

Plate structural element.

### Constructor
```julia
Plane(ndim, nst, nxe, nye, nip, dir, finite_element(nod, nodof), axisymmetric)
```

### Arguments
```julia
* ndim::Int               : Number of dimensions
* nst::Int                : Number of stress terms
* nxe::Int                : Number of elements in x direction
* nye::Int                : Number of elements in y direction
* nip::Int                : Number of integration points
* dir::Symbol             : Direction of node numbering
* fin_el::FiniteElement   : Line(nod, nodof)
* axisymmetric::Bool      : Axisymmetric
```

### Related help
```julia
?StructuralElement  : List structural elements
?FiniteElement      : List finite element types
?Line               : Help on a Line finite element
```
"""
mutable struct Plane <: StructuralElement
  ndim::Int                       # Number of dimensions (1,2 or 3)
  nst::Int                        # Number of stress terms
  nxe::Int                        # Number of fin_els in x direction
  nye::Int                        # Number of fin_els in y direction
  nip::Int                        # Number of integration points
  direction::Symbol               # Node numbering direction
  fin_el::FiniteElement           # Finite fin_el type used
  axisymmetric::Bool              # Axisymmetric
end

"""
## Solid

Solid structural element.

### Constructor
```julia
Solid(ndim, nst, nxe, nye, nze, nip, finite_element(nod, nodof))
```

### Arguments
```julia
* ndim::Int             : Number of dimensions
* nst::Int              : Number of stress terms
* nxe::Int              : Number of elements in x direction
* nye::Int              : Number of elements in y direction
* nze::Int              : Number of elements in z direction
* nip::Int              : Number of integration points
* fin_el::FiniteElement : Line(nod, nodof)
```

### Related help
```julia
?StructuralElement  : List structural elements
?FiniteElement      : List finite element types
```
"""
mutable struct Solid <: StructuralElement
  ndim::Int                     # Number of dimensions (1,2 or 3)
  nst::Int                      # Number of stress terms
  nxe::Int                      # Number of fin_els in x direction
  nye::Int                      # Number of fin_els in y direction
  nze::Int                      # Number of fin_els in y direction
  nip::Int                      # Number of integration points
  fin_el::FiniteElement         # Finite element type used
end

"""
## GenericSolid

Solid structural element.

### Constructor
```julia
GenericSolid(ndim, nst, nels, nn, nip, finite_element(nod, nodof), axisymmetric)
```

### Arguments
```julia
* ndim::Int               : Number of dimensions
* nst::Int                : Number of stress terms
* nels::Int               : Number of finite elements
* nn::Int                 : Number of nodes
* nip::Int                : Number of integration points
* fin_el::FiniteElement   : Finite element type used
* axisymmetric::Bool      : Axisymmetric
```

### Related help
```julia
?StructuralElement  : List structural elements
?FiniteElement      : List finite element types
```
"""
mutable struct GenericSolid <: StructuralElement
  ndim::Int                     # Number of dimensions (1,2 or 3)
  nst::Int                      # Number of stress terms
  nels::Int                     # Number of fin_els
  nn::Int                       # Number of nodes
  nip::Int                      # Number of integration points
  fin_el::FiniteElement         # Finite element type used
  axisymmetric::Bool            # Axisymmetric
end

struct UnknownStructuralElement <: StructuralElement
end

