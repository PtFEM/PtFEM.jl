using PtFEM
using Base.Test

# write your own tests here

if VERSION.minor == 3
  isfile("example.vtu") && rm("example.vtu")
  isfile("example_bin.vtu") && rm("example_bin.vtu")
  isfile("example_compressed_bin.vtu") && rm("example_compressed_bin.vtu")
end

code_tests = [
  "PtFEM.jl",
  "p41.1.jl",
  "p41.2.jl",
  "p42.2.jl",
  "p43.1b.jl",
  "p44.2.jl",
  "VTK.jl",
  "p45.2.jl",
  "p46.2.jl",
  "p47.1.jl",
  "p51.1.jl",
  "p51.2.jl",
  "p51.3.jl",
  "p51.4.jl",
  "p51.5.jl",
  "p52.1.jl",
  "p53.1.jl",
  "p54.1.jl",
  "p54.2.jl",
  "p55.1.jl",
  "p56.1.jl",
  "p61.1.jl",
  "p62.1.jl"
]

if isfile(Pkg.dir("PtFEM", "deps", "src", "PtFEM", "4th_ed", "libcsom.dylib"))
  push!(code_tests, "csom2.jl")
end

println("Running tests:")

for my_test in code_tests
    println("\n  * $(my_test) *")
    include("test_"*my_test)
end

println("\nPerforming Julia 0.6+ tests\n")
for my_test in code_tests
  println("\n  * $(my_test) *")
  include("test_"*my_test)
end
