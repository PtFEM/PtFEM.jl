function shootingmethod(f::Function, bvs::Vector{Float64}, initg::Vector{Float64}, steps::Int64, tol::Float64=0.00001, limit::Int64=25)
  println("\n--- Shooting method for Second Order ODEs ---\n")
  @assert size(bvs, 1) == 4
  @assert size(initg, 1) == 2
  nsteps = steps + 1
  y0 = zeros(Float64, nsteps, 2)
  ystar = zeros(Float64, nsteps)
  y = zeros(Float64, 2)
  (xa, ya, xb, yb) = bvs[:, 1]
  a0 = initg[:, 1]
  h = (xb - xa)/steps
  res = zeros(Float64, nsteps, 2)
  for j in 1:2
    x = xa
    y[1] = ya
    y[2] = a0[j]
    for i in 1:nsteps
      y0[i, j] = y[1]
      k0 = h * f(x, y)
      k1 = h * f(x+h/2, y+k0/2)
      k2 = h * f(x+h/2, y+k1/2)
      k3 = h * f(x+h, y+k2)
      y += (k0+2k1+2k2+k3)/6
      x = x + h
    end
  end
  if (y0[nsteps,1]-yb)*(y0[nsteps,2]-yb) > 0.0
    println("Try new gradients ... ?")
    return
  end
  iter = 0
  while true
    iter += 1
    astar = a0[1] + (yb-y0[nsteps, 1])*(a0[2]-a0[1])/(y0[nsteps,2])
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
      for i in 1:nsteps
        res[i,:] = [xa+(i-1)*h ystar[i]]
      end
      break
    end
    if (ystar[nsteps]-yb)*(y0[nsteps, 1]-yb) > 0.0
      y0[:, 1] = ystar
      a0[1] = astar
    else
      y0[:, 2] = ystar
      a0[2] = astar
    end
  end
  res
end
