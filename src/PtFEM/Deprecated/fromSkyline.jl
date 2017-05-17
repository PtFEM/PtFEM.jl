"""
## fromSkyline

Helper function to convert a Skyline vector to a full matrix.

### Type
```julia
fromSkyline(skyline::Vector{Float64}, kdiag::Vector{Int})
```

### Arguments
```julia
* skyline::Vector{Float64}     : 1D Line(nod, nodof)
* kdiag::Vector{Int}         : 2D Triangle(nod, nodof)
```
"""
function fromSkyline(skyline::Vector{Float64}, kdiag::Vector{Int})
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
	km
end
