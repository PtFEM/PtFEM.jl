using Compat

### Finite elements used to approximate fin_el ###

"""
## FiniteElement

Abstract finite element type.

### Type
```julia
abstract type FiniteElement end
```

### Subtypes
```julia
* Line::FiniteElement          : 1D Line(nod, nodof)
* Triangle::FiniteElement      : 2D Triangle(nod, nodof)
* Quadrilateral::FiniteElement : 2D Quadrilateral(nod, nodof)
* Hexahedron::FiniteElement    : 3D Hexahedron(nod, nodof)
* Tetrahedron::FiniteElement   : 3D Tetrahedron(nod, nodof)
```
"""
abstract type FiniteElement end

"""
## Line (Interval)

1D type finite element

### Constructor
```julia
Line(nod, nodof)
Line(nodof)
```

### Arguments
```julia
* nod::Int       : Number of nodes for finite element, defaults to 2
* nodof::Int     : Number of degrees of freedom per node
```

### Related help
```julia
?FiniteElement      : Help on finite element types
```

"""
struct Line <: FiniteElement
  nod::Int
  nodof::Int
end
Line(nodof::Int) = Line(2, nodof)

"""
## Triangle

2D type finite element

### Constructor
```julia
Triangle(nod, nodof)
```

### Arguments
```julia
* nod::Int       : Number of nodes for finite element (3, 6, 10, 15)
* nodof::Int     : Number of degrees of freedom per node
```

### Related help
```julia
?FiniteElement      : Help on finite element types
```

"""
struct Triangle <: FiniteElement
  nod::Int
  nodof::Int
end

"""
## Quadrilateral

2D type finite element

### Constructor
```julia
Quadrilateral(nod, nodof)
```

### Arguments
```julia
* nod::Int       : Number of nodes for finite element (4, 8, 9)
* nodof::Int     : Number of degrees of freedom per node
```

### Related help
```julia
?FiniteElement      : Help on finite element types
```

"""
struct Quadrilateral <: FiniteElement
  nod::Int
  nodof::Int
end

"""
## Tetrahedron

3D type finite element

### Constructor
```julia
Tetrahedron(nod, nodof)
Tetrahedron(nodof)
```

### Arguments
```julia
* nod::Int       : Number of nodes for finite element (defaults to 4)
* nodof::Int     : Number of degrees of freedom per node
```

### Related help
```julia
?FiniteElement      : Help on finite element types
```

"""
struct Tetrahedron <: FiniteElement
  nod::Int
  nodof::Int
end
Tetrahedron(nodof::Int) = Tetrahedron(4, nodof)

"""
## hexahedron

3D type finite element

### Constructor
```julia
Hexahedron(nod, nodof)
```

### Arguments
```julia
* nod::Int       : Number of nodes for finite element (8, 14, 20)
* nodof::Int     : Number of degrees of freedom per node
```

### Related help
```julia
?FiniteElement      : Help on finite element types
```

"""
struct Hexahedron <: FiniteElement
  nod::Int
  nodof::Int
end
