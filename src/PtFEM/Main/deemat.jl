"""
## deemat!

This subroutine returns the elastic dee matrix for ih=3 (plane strain),
ih=4 (axisymmetry or plane strain elastoplasticity) or ih=6
(three dimensions).

### Method
```julia
deemat!(dee, e, v)
```

### Arguments
```julia
* dee::Matrix{Float64}         : Dee matrix (Updated)
* e::Float64                   : Young's modulus
* v::Float64                   : Poisson's ratio
```
"""
function deemat!(dee::Array{Float64, 2}, e::Float64, v::Float64)
#
# This subroutine returns the elastic dee matrix for ih=3 (plane strain),
# ih=4 (axisymmetry or plane strain elastoplasticity) or ih=6
# (three dimensions).
#
  ih=size(dee,1)
  v1 = 1.0-v
  c = e/((1.0+v)*(1.0-2v))
  if ih == 3
    dee[1,1] = v1*c
    dee[2,2] = v1*c
    dee[1,2] = v*c
    dee[2,1] = v*c
    dee[3,3] = 0.5*c*(1.0-2v)
  elseif ih == 4
    dee[1,1] = v1*c
    dee[2,2] = v1*c
    dee[4,4] = v1*c
    dee[3,3] = 0.5*c*(1.0-2v) 
    dee[1,2] = v*c
    dee[2,1] = v*c
    dee[1,4] = v*c
    dee[4,1] = v*c
    dee[2,4] = v*c
    dee[4,2] = v*c
  elseif ih == 6
   v2 = v/(1.0-v)
   vv = (1.0-2v)/(1.0-v)*0.5
   for i in 1:3
    dee[i,i] = 1.0
   end
   for i in 4:6
     dee[i,i] = vv
   end
    dee[1,2] = v2
    dee[2,1] = v2
    dee[1,3] = v2
    dee[3,1] = v2
    dee[2,3] = v2
    dee[3,2] = v2
    dee[:, :] = dee*e/(2.0*(1.0+v)*vv)
  else
   println("wrong size for dee matrix")
  end
end