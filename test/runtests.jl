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
  "p4.1.1.jl",
  "p4.1.2.jl",
  "p4.2.2.jl",
  "p4.3.1b.jl",
  "p4.4.2.jl",
  "VTK.jl"
]

code_tests_0_5 = [
  "p4.5.2_0_5.jl",
  "p4.6.2_0_5.jl",
  "p4.7.1_0_5.jl",
  "p5.1.1_0_5.jl",
  "p5.1.2_0_5.jl",
  "p5.1.3_0_5.jl",
  "p5.1.4_0_5.jl",
  "p5.1.5_0_5.jl",
  "p5.2.1_0_5.jl",
  "p5.3.1_0_5.jl",
  "p5.4.1_0_5.jl",
  "p5.4.2_0_5.jl",
  "p5.5.1_0_5.jl",
  "p5.6.1_0_5.jl",
  "p6.1.1_0_5.jl"
]

code_tests_0_6 = [
  "p4.5.2.jl",
  "p4.6.2.jl",
  "p4.7.1.jl",
  "p5.1.1.jl",
  "p5.1.2.jl",
  "p5.1.3.jl",
  "p5.1.4.jl",
  "p5.1.5.jl",
  "p5.2.1.jl",
  "p5.3.1.jl",
  "p5.4.1.jl",
  "p5.4.2.jl",
  "p5.5.1.jl",
  "p5.6.1.jl",
  "p6.1.1.jl",
]

if isfile(Pkg.dir("PtFEM", "deps", "src", "PtFEM", "4th_ed", "libcsom.dylib"))
  push!(code_tests, "csom2.jl")
end

println("Running tests:")

for my_test in code_tests
    println("\n  * $(my_test) *")
    include("test_"*my_test)
end

if VERSION.minor == 5
  println("\nPerforming Julia 0.5 tests\n")
  for my_test in code_tests_0_5
    println("\n  * $(my_test) *")
    include("test_"*my_test)
  end
end
if VERSION.minor >= 6
  println("\nPerforming Julia 0.6+ tests\n")
  for my_test in code_tests_0_6
    println("\n  * $(my_test) *")
    include("test_"*my_test)
  end
end  