@doc doc"""
## formnf!

...

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
function formnf!(nodof::Int64, nn::Int64, nf::Matrix{Int64})
  local m = 0
  for j in 1:nn
    for i in 1:nodof
      if nf[i,j] != 0
        m += 1
        nf[i,j] = m
      end
    end
  end
end
