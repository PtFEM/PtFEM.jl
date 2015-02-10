function euler(f::Function, x::Float64, y::Vector{Float64}, steps::Int64, h::Float64)
  nsteps = steps + 1
  res = zeros(nsteps, length(y)+1)
  for i in 1:nsteps
    res[i,:] = [x; [y[i] for i in 1:length(y)]]
    k0 = h * f(x, y)
    y += k0
    x += h
  end
  res
end

function modified_euler(f::Function, x::Float64, y::Vector{Float64}, steps::Int64, h::Float64)
  nsteps = steps + 1
  res = zeros(nsteps, length(y)+1)
  for i in 1:nsteps
    res[i,:] = [x; [y[i] for i in 1:length(y)]]
    k0 = h * f(x, y)
    k1 = h * f(x+h, y+k0)
    y += (k0+k1)/2.0
    x += h
  end
  res
end

function mid_point_euler(f::Function, x::Float64, y::Vector{Float64}, steps::Int64, h::Float64)
  nsteps = steps + 1
  res = zeros(nsteps, length(y)+1)
  for i in 1:nsteps
    res[i,:] = [x; [y[i] for i in 1:length(y)]]
    k0 = h * f(x, y)
    k1 = h * f(x+h/2.0, y+k0/2.0)
    y += k1
    x += h
  end
  res
end

function runga_kutta_4(f::Function, x::Float64, y::Vector{Float64}, steps::Int64, h::Float64)
  nsteps = steps + 1
  res = zeros(nsteps, length(y)+1)
  for i in 1:nsteps
    res[i,:] = [x; [y[i] for i in 1:length(y)]]
    k0 = h * f(x, y)
    k1 = h * f(x+h/2.0, y+k0/2.0)
    k2 = h * f(x+h/2.0, y+k1/2.0)
    k3 = h * f(x+h/2.0, y+k2)
    y += (k0+2.0*k1+2.0*k2+k3)/6.0
    x += h
  end
  res
end
