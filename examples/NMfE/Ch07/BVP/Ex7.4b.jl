using CSoM

old = pwd()
ProjDir = Pkg.dir("CSoM", "examples", "NMfE", "Ch07", "BVP")
cd(ProjDir)

function f(x::Float64, y::Vector{Float64})
  [y[2], -2y[2]/y[1]]
end

res = shootingmethod(f, [0.0, 1.0, 1.0, 2.0], [1.0, 3.0], 10, 0.00001, 40)
@show round(res, 4)

cd(old)