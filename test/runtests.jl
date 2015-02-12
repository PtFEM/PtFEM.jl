using CSoM
using Base.Test

# write your own tests here

code_tests = [
  "test_csom1.jl",
  "test_p5.1.jl",
  "test_lufac.jl",
  "test_ldlt.jl",
  "test_ivp_ex7.3.jl",        # Four direct methods
  "test_bvp_ex7.4a.jl",       # Shooting method example 7.4a
  "test_bvp_ex7.4b.jl"        # Shooting method example 7.4b
]

if isfile(Pkg.dir("CSoM", "deps", "src", "CSoM", "4th_ed", "libcsom.dylib"))
  push!(code_tests, "test_csom2.jl")
end

println("Running tests:")

for my_test in code_tests
    println("\n  * $(my_test) *")
    include(my_test)
end

