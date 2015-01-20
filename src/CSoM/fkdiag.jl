function fkdiag!(ndof::Int64, neq::Int64, g::Vector{Int64}, kdiag::Vector{Int64})
  for i in 1:ndof
    imp1 = 1
    if g[i] !== 0
      for j in 1:ndof
        if g[j] !== 0
          im = g[i] - g[j] + 1
          if im > imp1
            imp1 = im
          end
        end
        k = g[i]
        if imp1 > kdiag[k]
          kdiag[k] = imp1
        end
      end
    end
  end
end
