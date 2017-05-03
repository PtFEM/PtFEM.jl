"""
## beemat!

This subroutine forms the strain-displacement matrix for
axisymmetric solids subjected to non-axisymmetric loading.

### Method
```julia
beemat!(bee, deriv)
```

### Arguments
```julia
* bee::Matrix{Float64}         : Bee matrix (Updated)
* deriv::Matrix{Float64}       : Derivative
```
"""
function beemat!(bee::Matrix{Float64},deriv::Matrix{Float64})
  #
  # This subroutine forms the bee matrix in 2-d (ih=3 or 4) or 3-d (ih=6).
  #
  ih = size(bee,1)
  nod = size(deriv,2)
  if ih == 3 || ih == 4
   for m in 1:nod
     k = 2m
     l = k-1
     x = deriv[1,m]
     y = deriv[2,m]
     bee[1,l] = x
     bee[3,k] = x
     bee[2,k] = y
     bee[3,l] = y
   end
  elseif ih == 6
   for m in 1:nod
     n = 3m
     k = n-1
     l = k-1
     x = deriv[1,m]
     y = deriv[2,m]
     z = deriv[3,m]
     bee[1,l] = x
     bee[4,k] = x
     bee[6,n] = x
     bee[2,k] = y
     bee[4,l] = y
     bee[5,n] = y
     bee[3,n] = z
     bee[5,k] = z
     bee[6,l] = z
   end
  else
   println("Wrong dimension for nst in bee matrix.")
  end   
end