using SparseVectors

m = reshape(full(sprand(25, 0.3)), 5, 5)
m |> display
println()

sparse(m)

#=
fromSkyline <- function(skyline, kdiag) {
	neq <- length(kdiag)
	km <- Matrix(0, nrow=neq, ncol=neq)
	km[1, 1] <- skyline[kdiag[1]]
	for (i in 2:length(kdiag)) {
		km[i, i] <- skyline[kdiag[i]]
		for (j in (kdiag[i-1] + 1):kdiag[i]) {
			km[i, i - (kdiag[i]-j)] <- skyline[j]
			km[i - (kdiag[i]-j), i] <- skyline[j]
		}
	}
	km
}
=#

function fromSkyline(skyline::Vector{Float64}, kdiag::Vector{Int64})
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

include("/Users/rob/.julia/v0.4/CSoM/examples/ElasticSolids/p5.1.1.jl")

fromSkyline(m.kv, m.kdiag)

