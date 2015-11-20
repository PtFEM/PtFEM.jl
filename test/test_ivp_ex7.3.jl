using CSoM

old = pwd()
ProjDir = Pkg.dir("CSoM", "examples", "NMfE", "Ch07", "IVP")
cd(ProjDir)

f(x::Float64, y::Vector{Float64}) = [y[2], 2.0*y[1]-3.0*y[2]+3.0*x^2]
steps = 4
h = 0.05

x = 0.0
y = [1.0, 0.0]


r = Array{Float64,2}[]
push!(r, euler(f, x, y, steps, h))
push!(r, modified_euler(f, x, y, steps, h))
push!(r, mid_point_euler(f, x, y, steps, h))
push!(r, runga_kutta_4(f, x, y, steps, h))

@assert r[1][steps+1,:][:] == [0.2, 1.0272471874999998, 0.3254353125]
@assert r[2][steps+1,:][:] == [0.2, 1.0338953695013426, 0.3110140747716065]
@assert r[3][steps+1,:][:] == [0.2, 1.033871576010437, 0.3107074949932251]
@assert r[4][steps+1,:][:] == [0.2, 1.033615605146455, 0.3112553545583527]

cd(old)