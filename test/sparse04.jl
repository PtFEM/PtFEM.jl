using Compat

if VERSION.minor == 7
  @eval using LinearAlgebra, SparseMatrices
end

ProjDir = dirname(@__FILE__)
cd(ProjDir) do

A = Float64[
  1/2 1/3 1/4 1/5 1/6;
  1/3 1/4 1/5 1/6 1/7;
  1/4 1/5 1/6 1/7 1/8;
  1/5 1/6 1/7 1/8 1/9;
  1/6 1/7 1/8 1/9 1/10;
]
b = ones(5)

(Al, Au, Ap) = lu(A)

@show y = Al \ b[Ap]
println()

@show x = Au \ y
println()

@show As = sparse(A)
println()

@show As\b
println()

@show F = lufact(As)
println()

if VERSION.minor == 7
  @show F.L * F.U == F.Rs .* A[F.p, :]
  println()
  
  @show F.L*F.U
  println()

  @show sparse(F.Rs.* A[F.p, F.q])
  println()
else
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
end

@show lufact(A)
println()

end