"""
## mesh_size

Function mesh_size returns the number of fin_els (nels) and the number
of nodes (nn) in a 1, 2 or 3-d geometry-created mesh.

### Method
```julia
(nels, nn) = mesh_size(fin_el, nxe, [nye[, nze]])
```

### Arguments
```julia
* fin_el::FiniteElement   : Shape of finite element
                            1D: Line
                            2D: Trangle or Quadrilateral
                            3D: Hexahedron
* nxe::Int              : Number of fin_els in x direction
* nye::Int              : Number of fin_els in y direction (for 2D and 3D)
* nze::Int              : Number of fin_els in z direction (3D only)
```
"""
function mesh_size(fe::Line, nxe::Int)
  nn=nxe+1; nels=nxe
  if fe.nod != 2
    println("Invalid number of nodes for Line fin_el.")
  end
  (nels, nn)
end

"""
# mesh_size

mesh_size: The function mesh_size returns the number of fin_els (nels) and the number
of nodes (nn) in a 2-d geometry-created mesh.

### Method
```julia
(nels, nn) = mesh_size(fin_el, nxe)
```
### Arguments
```julia
* `fin_el` : Shape of 2D finite element (Triangle)
* `nxe` : Number of fin_els in x direction
* `nxe` : Number of fin_els in y direction
```
"""
function mesh_size(fe::Triangle, nxe::Int, nye::Int)
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
    println("Invalid number of nodes for Triangle fin_el.")
  end
  (nels, nn)
end

"""
# mesh_size

mesh_size: The function mesh_size returns the number of fin_els (nels) and the number
of nodes (nn) in a 2-d geometry-created mesh.

### Method
```julia
(nels, nn) = mesh_size(fin_el, nxe, nye)
```
### Arguments
```julia
* `fin_el` : Shape of 2D finite element (Quadrilateral)
* `nxe` : Number of fin_els in x direction
* `nye` : Number of fin_els in y direction
```
"""
function mesh_size(fe::Quadrilateral, nxe::Int, nye::Int)
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
    println("Invalid number of nodes for Quadrilateral fin_el.")
  end
  (nels, nn)
end

"""
# mesh_size

mesh_size: The function mesh_size returns the number of fin_els (nels) and the number
of nodes (nn) in a 3-d geometry-created mesh.

### Method
```julia
(nels, nn) = mesh_size(fin_el, nxe, nye, nze)
```
### Arguments
```julia
* `fin_el` : Shape of 2D finite element (Hexahedron)
* `nxe` : Number of fin_els in x direction
* `nye` : Number of fin_els in y direction
* `nxe` : Number of fin_els in x direction
```
"""
function mesh_size(fe::Hexahedron, nxe::Int, nye::Int, nze::Int)
  nn=0; nels=nxe*nye*nze
  if fe.nod==8
    nn=(nxe+1)*(nye+1)*(nze+1)
  elseif fe.nod==14
    nn=4*nxe*nye*nze+2*(nxe*nye+nye*nze+nze*nxe)+nxe+nye+nze+1
  elseif fe.nod==20
    nn=((2*nxe+1)*(nze+1)+(nxe+1)*nze)*(nye+1)+(nxe+1)*(nze+1)*nye
  else
    println("Invalid number of nodes for Hexahedron fin_el.")
  end
  (nels, nn)
end
