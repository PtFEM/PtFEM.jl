function lufac(a::Matrix{Float64})
  upper = zeros(size(a))
  upper[1,:] = a[1,:]
  lower = eye(size(a, 1))
  const n = size(a, 1)
  for k in 1:n-1
    if abs(upper[k,k] > 1e-10)
      for i in k+1:n
        # L component
        for j in 1:i-1
          total = 0.0
          for l in 1:j-1
            total = total - lower[i, l] * upper[l, j]
          end
          lower[i, j] = (a[i, j] + total)/upper[j, j]
        end
        # U component
        for j in 1:n
          total = 0.0
          for l in 1:i-1
            total = total - lower[i, l] * upper[l, j]
          end
          upper[i, j] = a[i, j] +total
        end
      end
    else
      println("Zero pivot found in row $k")
    end
  end
  (lower, upper)
end
