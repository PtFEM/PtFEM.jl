A = Float64[10 1 -5;-20 3 20;5 3 5]
b = Float64[1; 2; 6]

(Al, Au, Ap) = lu(A)

@show y = Al \ b[Ap]
println()

@show x = Au \ y
println()

@show As = sparse(A)
println()

@show As\l
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

@show PtFEM.lufac(A)
println()

cd(old)