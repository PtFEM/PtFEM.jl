using CSoM

A = Float64[3 -2 1;-2 3 2;1 2 2]
b = Float64[3, -3, 2]

L, U = lufac(A)

@assert round(U, 5) == [3.0 -2.0 1.0;0.0 1.66667 2.66667;0.0 0.0 -2.6]

#=
function testlufac(a::Matrix{Float64}, n::Int64=int(1e6))
  for i in 1:n
    lufac(a)
  end
end

function testlu(a::Matrix{Float64}, n::Int64=int(1e6))
  for i in 1:n
    lu(a)
  end
end

@time testlufac(A)
@time testlu(A)
=#