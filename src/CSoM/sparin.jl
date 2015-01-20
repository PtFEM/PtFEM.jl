#=
@doc doc"""
  This subroutine performs Cholesky factorisation on a symmetric
  skyline global matrix.

  sparin(kv, kdiag)

  where:
    kv::Vector{Float64}   : Skyline vector of global stiffness matrix
    kdiag::Vector{Int64}  : Diagonal element vector
  """ ->
=#
function sparin!(kv::Vector{Float64}, kdiag::Vector{Int64})
  local x::Float64
  n = size(kdiag, 1)
  kv[1] = sqrt(kv[1])
  for i in 2:n
    ki = kdiag[i]-i
    l = kdiag[i-1] - ki + 1
    for j in l:i
      x = kv[ki+j]
      kj = kdiag[j] - j
      if j !== 1
        ll = kdiag[j-1] - kj + 1
        ll = max(l, ll)
        if ll !== j
          m = j - 1
          for k in ll:m
            x = x - kv[ki+k]*kv[kj+k]
          end
        end
      end
      kv[ki+j] = x/kv[kj+j]
    end
    kv[ki+i] = sqrt(x)
  end
end

