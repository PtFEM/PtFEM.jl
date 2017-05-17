"""
## fkdiag!

This subroutine returns the elastic dee matrix for ih=3 (plane strain),
ih=4 (axisymmetry or plane strain elastoplasticity) or ih=6
(three dimensions).

### Method
```julia
fkdiag!(kdiag, g)
```

### Arguments
```julia
* kdiag::Vector{Int}      : Bandwidth vector (Updated)
* g::Vector{Int}          : Element steering vector
```
"""
function fkdiag!(kdiag::Vector{Int}, g::Vector{Int})
  idof = size(g, 1)
  for i in 1:idof
    imp1 = 1
    if g[i] !== 0
      for j in 1:idof
        if g[j] !== 0
          im = g[i] - g[j] + 1
          if im > imp1
            imp1 = im
          end
        end
        k = g[i]
        if imp1 > kdiag[k]
          kdiag[k] = imp1
        end
      end
    end
  end
end
