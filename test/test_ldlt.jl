A = Float64[16 4 8;4 5 -4;8 -4 22]
b = Float64[4, 2, 5]

A\b |> display
println()

@show D = ldlt(A)
println()

#include("lufac.jl")
@show (L, U) = lufac(A)
println()

@show y = L\b
println()

@show 

function testldlt(a::Matrix{Float64}, n::Int64=int(1e6))
  for i in 1:n
    ldlt(a)
  end
end

@time testldlt(A)
