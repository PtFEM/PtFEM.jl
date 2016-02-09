A = Float64[
  16 4 8;
  4 5 -4;
  8 -4 22;
]
a = [16; 18; -22]

F = chol(A, Val{:L})

@show y = F \ a
println()

@show x = F' \ y
println()

@show As = sparse(A)
println()

@show As\a
println()

B = copy(A)
b = copy(a)

B[2,2] += 1.0e10
b[2] = (5 + 1.0e10) * 5.0

F = chol(B, Val{:L})

@show y = F \ b
println()

@show x = F' \ y
println()

@show Bs = sparse(B)
println()

@show Bs\b
println()
