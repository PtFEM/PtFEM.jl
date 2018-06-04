using PtFEM
using Test

# write your own tests here

code_tests = [
  "p41.1.jl",
  "p41.2.jl",
  "p42.2.jl",
  "p43.1b.jl",
  "p44.2.jl",
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
  "p62.1.jl",
  "p63.1.jl"
]

println("\n\nRunning PtFEM/PtFEM.jl tests:\n\n")

@testset "PtFEM.jl" begin
  for test in code_tests
      println("\n\n  * $(test) *\n")
      include("test_"*test)
  end
end