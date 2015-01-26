using CSoM

old = pwd()
ProjDir = Pkg.dir("CSoM", "examples", "NMfE", "Ch07", "BVP")
cd(ProjDir)

function f(x::Float64, y::Vector{Float64})
  [y[2], 3x^2+4y[1]]
end

res = shootingmethod(f, [0.0, 0.0, 1.0, 1.0], [0.0, 1.0], 5)

@assert round(res[:, 2], 4)' == [0.0  0.0813  0.1815  0.3313  0.5794  1.0]

cd(old)