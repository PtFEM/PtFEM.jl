using CSoM

old = pwd()
ProjDir = Pkg.dir("CSoM", "examples", "NMfE", "Ch07", "BVP")
cd(ProjDir)

f(x::Float64, y::Vector{Float64}) = (x + y[1])/x
steps = 10
h = 0.1
function shoot(f::Function, init::Vector{Float64}, steps::Int64, tol::Float64=0.00001, limit::Int64=25)
  nsteps = steps + 1
  y0 = zeros(Float64, nsteps, 2)
  ystar = zeros(Float64, nsteps)
  println("--- Shooting method for Second Order ODEs ---")
  @assert length(init) == 4
  (xa, ya, xb, yb) = init
  h = (xb - xa)/nsteps
  for j in 1:2
    x = xa
    y[1] = ya
    for i in 1:nsteps
      y0[i, j] = y[1]
      k0 = h * f(x, y)
      k1 = h * f(x+h/2, y+k0/2)
      k2 = h * f(x+h/2, y+k1/2)
      k3 = h * f(x+h, y+k2)
      y += (k0+2k1+2k2+k3)/6
      x = x + h
    end
    if (y0[nsteps,1]-yb)*(y0[nsteps,2]-yb) > 0.0
      println("Try new gradients ...?")
    end
    iter = 0
    while true
      iters += 1
      astar = a0[1] + (yb-y0[nsteps, 1])*(ao[2]-a0[1])/(yo[nsteps,2])
      x = xa
      y[1] = ya
      y[2] = astar
      for i in 1:nsteps
        ystar[i] = y[1]
        k0 = h * f(x, y)
        k1 = h * f(x+h/2, y+k0/2)
        k2 = h * f(x+h/2, y+k1/2)
        k3 = h * f(x+h, y+k2)
        y += (k0+2k1+2k2+k3)/6
        x = x + h
      end
      if abs((ystar[nsteps]-yb)/yb) < tol
        println("Iterations to convergence: $iter")
        
        break
      end
    end
  end
end



cd(old)