function ldlt(ain::Matrix{Float64})
  a = copy(ain)
  n = size(a,1)
  d = zeros(n)
  for k in 1:n-1
    d[1] = a[1,1]
    if abs(a[k,k] > 1e-10)
      for i in k+1:n
        x=a[i,k]/a[k,k]
        for j=k+1:n
          a[i,j]=a[i,j]-a[k,j]*x
        end
        d[i]=a[i,i]
      end
    else
      println("Zero pivot found in row $k")
    end
  end
  d
end
