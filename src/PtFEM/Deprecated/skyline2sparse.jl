"""
## skyline2sparse

Converts a Skyline matrix to a Julia Sparse matrix

### Function
```julia
skyline2sparse(skyline, kdiag)
```

### Arguments
```julia
* skyline::Vector{Float64}         : Skyline matrix
* kdiag::Vector{Int}             : Element diagonal index vector
```
"""
function skyline2sparse(skyline::Vector{Float64}, kdiag::Vector{Int})
	neq = size(kdiag, 1)
	km = zeros(neq, neq)
	km[1, 1] = skyline[kdiag[1]]
	for i in 2:neq
		km[i, i] = skyline[kdiag[i]]
		for j in (kdiag[i-1] + 1):kdiag[i]
			km[i, i - (kdiag[i]-j)] = skyline[j]
			km[i - (kdiag[i]-j), i] = skyline[j]
    end
  end
	sparse(km)
end
