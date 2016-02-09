using IterativeSolvers

A = Float64[
  16 4 8;
  4 5 -4;
  8 -4 22;
]
a = Float64[16; 18; -22]
x = Float64[1; 1; 1]

maxiters = 100
println("\nJacobi:")
for i in 1:100
  res = jacobi!(x, A, a, tol=1.0e-8, maxiter=1)
  if i < 5 || i in [1; 10:10:maxiters]
    println([i x'])
  end
  if res[2].isconverged
    println([i x'])
    break
  end
end

println("\nGauss-Seidel:")
@show  A * x
println()

x = Float64[1; 1; 1]
for i in 1:100
  res = gauss_seidel!(x, A, a, tol=1.0e-8, maxiter=1)
  if i < 5 || i in [1; 10:10:maxiters]
    println([i x'])
  end
  if res[2].isconverged
    println([i x'])
    break
  end
end

println("\nSequential Succesive Over-Relaxation:")
@show A * x
println()

x = Float64[1; 1; 1]
ω = 1.0
for i in 1:100
  res = ssor!(x, A, a, ω, tol=1.0e-8, maxiter=1)
  if i < 5 || i in [1; 10:10:maxiters]
    println([i x'])
  end
  if res[2].isconverged
    println([i x'])
    break
  end
end

println()
A * x
