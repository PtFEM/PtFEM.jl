using Compat

if VERSION.minor == 7
  @eval using LinearAlgebra, SparseMatrices
end

ProjDir = dirname(@__FILE__)
cd(ProjDir) do

A = Float64[10 1 -5;-20 3 20;5 3 5]
b = Float64[1; 2; 6]

(Al, Au, Ap) = lu(A)

@show y = Al \ b[Ap]
println()

@show x = Au \ y
println()

@show As = sparse(A)
println()

@show As \ y
println()

@show F = lufact(As)
println()

@show ys = F[:L] \ (F[:Rs] .* b)
println()

@show x = F[:U] \ ys
println()

@show F[:L]*F[:U] == (F[:Rs] .* A)[F[:p], F[:q]]
println()

@show F[:L]*F[:U]
println()

@show sparse((F[:Rs] .* A)[F[:p], F[:q]])
println()

@show lufact(A)
println()

F = lufact(A)

if VERSION.minor == 7
  F.L * F.U == A[p, :]
else
  F[:L]* F[:U] == A[F[:p], :]
end
  

end