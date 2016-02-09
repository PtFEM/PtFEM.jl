function bicgstab(a::Matrix{Float64}, b::Vector{Float64}, x0::Vector{Float64}; 
    tol=1.0e-5, limit=1000, trace=false)
  n = size(a, 2)
  if length(b) != n || length(x0) != n
    println("Non-conformant sizes of inputs.")
  end
  
  x = copy(x0)
  r = b - a * x
  r0_hat = r
  
  rho0 = 1.0
  alpha = 1.0
  w = 1.0
  v = zeros(n)
  p = zeros(n)
  rho1 = dot(r0_hat, r)
  
  iters = 0
  converged = false
  while true
    iters = iters + 1
    converged = norm(r) < tol * norm(b)
    if iters == limit || converged
      if trace
        println([iters norm(r) norm(b) x[1:min(5, length(x0))]'])
      end
      break
    end
    beta = (rho1 / rho0) * (alpha / w)
    p = r + beta * (p - w * v)
    v = a * p
    alpha = rho1 / dot(r0_hat, v)
    s = r - alpha * v
    t = a * s
    w = dot(t, s) / dot(t, t)
    rho0 = rho1
    rho1 = -w * dot(r0_hat, t)
    x = x + alpha * p + w * s
    r = s - w * t
    if trace
      if iters in [1; 2; 3; 50:50:limit]
        println([iters norm(r) norm(b) x[1:min(5, length(x0))]'])
      end
    end
  end
  (iters, converged, x)
end
