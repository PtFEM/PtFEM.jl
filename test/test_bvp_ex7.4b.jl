using CSoM

old = pwd()
ProjDir = Pkg.dir("CSoM", "examples", "NMfE", "Ch07", "BVP")
cd(ProjDir)

function f(x::Float64, y::Vector{Float64})
  [y[2], -2y[2]/y[1]]
end

res = shootingmethod(f, [0.0, 1.0, 1.0, 2.0], [1.0, 3.0], 10)

@assert round(res[:, 2], 4)' == [1.0  1.1742  1.321  1.4471  1.5569  1.6534  1.7389  1.815  1.8833  1.9446  2.0]

cd(old)