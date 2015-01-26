using CSoM
using Base.Test

# write your own tests here

code_tests = [
  "test_csom1.jl",
  "test_lufac.jl",
  "test_ldlt.jl",
  "test_ivp_ex7.3.jl"
]

if isdir(Pkg.dir("CSoM", "deps"))
  push!(code_tests, "test_csom2.jl")
end

println("Running tests:")

for my_test in code_tests
    println("\n  * $(my_test) *")
    include(my_test)
end

