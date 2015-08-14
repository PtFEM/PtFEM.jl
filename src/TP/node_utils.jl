function n2d(v::Vector{Int64})
  nv = zeros(Int64, maximum(v))
  for i in 1:maximum(v)
    r = findin(v, [i])
    nv[i] = size(r, 1) == 0 ? 0 : r[1]
  end
  nv
end