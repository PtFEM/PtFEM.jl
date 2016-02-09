function skyline2sparse(skyline::Vector{Float64}, kdiag::Vector{Int64})
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
