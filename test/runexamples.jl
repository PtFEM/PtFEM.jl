using PtFEM
using Base.Test

# write your own tests here

if VERSION.minor == 3
  isfile("example.vtu") && rm("example.vtu")
  isfile("example_bin.vtu") && rm("example_bin.vtu")
  isfile("example_compressed_bin.vtu") && rm("example_compressed_bin.vtu")
end

examples = [
  "PtFEM/4 Static Equilibrium/Ex41.1.jl",
  "PtFEM/4 Static Equilibrium/Ex41.2.jl",
  "PtFEM/4 Static Equilibrium/Ex42.2.jl",
  "PtFEM/4 Static Equilibrium/Ex43.1b.jl",
  "PtFEM/4 Static Equilibrium/Ex44.2.jl",
  "PtFEM/4 Static Equilibrium/Ex45.2.jl",
  "PtFEM/4 Static Equilibrium/Ex46.2.jl",
  "PtFEM/4 Static Equilibrium/Ex47.1.jl",
  "PtFEM/5 Elastic Solids/Ex51.1.jl",
  "PtFEM/5 Elastic Solids/Ex51.2.jl",
  "PtFEM/5 Elastic Solids/Ex51.3.jl",
  "PtFEM/5 Elastic Solids/Ex51.4.jl",
  "PtFEM/5 Elastic Solids/Ex51.5.jl",
  "PtFEM/5 Elastic Solids/Ex52.1.jl",
  "PtFEM/5 Elastic Solids/Ex53.1.jl",
  "PtFEM/5 Elastic Solids/Ex54.1.jl",
  "PtFEM/5 Elastic Solids/Ex54.2.jl",
  "PtFEM/5 Elastic Solids/Ex55.1.jl",
  "PtFEM/5 Elastic Solids/Ex56.1.jl",
  #"PtFEM/5 Elastic Solids/Ex57.1.jl",        # Takes a while ...
  #"PtFEM/5 Elastic Solids/Ex57.1b.jl",       # Requires too much memory
  "PtFEM/6 Material Nonlinearity/Ex61.1.jl",
  "PtFEM/6 Material Nonlinearity/Ex62.1.jl"
]

println("\nRunning examples:\n")

if VERSION.minor == 5
  println("\nRunning Julia 0.5 examples\n")
  for ex in examples
    println("\n\nRunning $(ex).\n\n")
    include(Pkg.dir("PtFEM")*"/examples/$(ex)")
  end
end
