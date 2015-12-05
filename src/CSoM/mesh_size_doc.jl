using Docile, Lexicon

Docile.@comment """
# mesh_size
"""

"""
mesh_size: The function mesh_size returns the number of elements (nels) and the number
of nodes (nn) in a 1-d geometry-created mesh.

### Method
```julia
(nels, nn) = mesh_size(element, nxe)
```
### Arguments
```julia
* `element` : Shape of 1D finite element (Line)
* `nxe` : Number of elements in x direction
```
"""
function mesh_size(fe::Line, nxe::Int64)
  nn=nxe+1; nels=nxe
  if fe.nod != 2
    println("Invalid number of nodes for Line element.")
  end
  (nels, nn)
end

Docile.@comment """
# mesh_size
"""

"""
mesh_size: The function mesh_size returns the number of elements (nels) and the number
of nodes (nn) in a 2-d geometry-created mesh.

### Method
```julia
(nels, nn) = mesh_size(element, nxe)
```
### Arguments
```julia
* `element` : Shape of 2D finite element (Triangle)
* `nxe` : Number of elements in x direction
* `nxe` : Number of elements in y direction
```
"""
function mesh_size(fe::Triangle, nxe::Int64, nye::Int64)
  nn=0; nels=nxe*nye*2
  if fe.nod==3
    nn=(nxe+1)*(nye+1)
  elseif fe.nod==6
    nn=(2*nxe+1)*(2*nye+1)
  elseif fe.nod==10
    nn=(3*nxe+1)*(3*nye+1)
  elseif fe.nod==15
    nn=(4*nxe+1)*(4*nye+1)
  else
    println("Invalid number of nodes for Triangle element.")
  end
  (nels, nn)
end

Docile.@comment """
# mesh_size
"""

"""
mesh_size: The function mesh_size returns the number of elements (nels) and the number
of nodes (nn) in a 2-d geometry-created mesh.

### Method
```julia
(nels, nn) = mesh_size(element, nxe, nye)
```
### Arguments
```julia
* `element` : Shape of 2D finite element (Quadrilateral)
* `nxe` : Number of elements in x direction
* `nye` : Number of elements in y direction
```
"""
function mesh_size(fe::Quadrilateral, nxe::Int64, nye::Int64)
  nn=0; nels=nxe*nye
  if fe.nod==4
    nn=(nxe+1)*(nye+1)
  elseif fe.nod==5
    nn=(nxe+1)*(nye+1)+nxe*nye
  elseif fe.nod==8
    nn=(2*nxe+1)*(nye+1)+(nxe+1)*nye
  elseif fe.nod==9
    nn=(2*nxe+1)*(2*nye+1)
  else
    println("Invalid number of nodes for Quadrilateral element.")
  end
  (nels, nn)
end

Docile.@comment """
# mesh_size
"""

"""
mesh_size: The function mesh_size returns the number of elements (nels) and the number
of nodes (nn) in a 3-d geometry-created mesh.

### Method
```julia
(nels, nn) = mesh_size(element, nxe, nye, nze)
```
### Arguments
```julia
* `element` : Shape of 2D finite element (Hexahedron)
* `nxe` : Number of elements in x direction
* `nye` : Number of elements in y direction
* `nxe` : Number of elements in x direction
```
"""
function mesh_size(fe::Hexahedron, nxe::Int64, nye::Int64, nze::Int64)
  nn=0; nels=nxe*nye*nze
  if fe.nod==8
    nn=(nxe+1)*(nye+1)*(nze+1)
  elseif fe.nod==14
    nn=4*nxe*nye*nze+2*(nxe*nye+nye*nze+nze*nxe)+nxe+nye+nze+1
  elseif fe.nod==20
    nn=((2*nxe+1)*(nze+1)+(nxe+1)*nze)*(nye+1)+(nxe+1)*(nze+1)*nye
  else
    println("Invalid number of nodes for Hexahedron element.")
  end
  (nels, nn)
end
