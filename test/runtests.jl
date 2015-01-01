using LHR
using Base.Test

# write your own tests here

code_tests = [
  "test_csom1.jl",
  "test_csom2.jl",
  "../examples/Beams/beam01.jl"
]

println("Running tests:")

for my_test in code_tests
    println("\n  * $(my_test) *")
    include(my_test)
end

