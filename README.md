# CSoM


[![Build Status](https://travis-ci.org/goedman/CSoM.jl.svg?branch=master)](https://travis-ci.org/goedman/CSoM.jl)


##Access to CSoM models

These are (some of) the programs as described in "Programming the Finite Element Method" by I M Smith, D V Griffiths and L. Margetts. The package should probably be called PtFEM.jl, but for now I have always related them to Colorado School of Mines (being a mining engineer from Delft University myself), hence [CSoM.jl](https://github.com/goedman/CSoM.jl). When I refer to the book zI use PtFEM, when I refer to the Julia package I use CSoM.

To use the toolkit and run several test programs, start the Julia REPL and type:

```
Pkg.clone("git@github.com:goedman/CSoM.jl")
Pkg.test("CSoM")
```

Example programs are in the examples and notebooks subdirectories.

Initial focus has been on chapters 4 to 6 of PtFEM in order to figure out how to best structure the toolkit. Currently all 'basic' functions are in the src/CSoM directory while higher level 'computational flow templates' are in src/Chapxx directories. In PtFEM no such intermediate template level is used. The idea of the templates is that they can be used to quickly setup similar models (but the jury is still out which approach is better).

The basic functions are also being reviewed for better approaches in [Julia](http://julialang.org) vs. the current 'translated-from-Fortran' flavor. Examples of these kind of changes are dropping the idea of skyline storage and directly using Julia sparse matrices and replacing the pair sparin() and spabac() by Julia's cholfact() and '\' operation, i.e. in FE4.1

replacing

```
  CSoM.sparin!(kv, kdiag)
  loads[2:end] = CSoM.spabac!(kv, loads[2:end], kdiag)
```

by

```
  cgsm = cholfact(gsm)
  loads[2:end] = cgsm \ loads[2:end]
```

Again, while experimenting with the proper structure of the CSoM toolkit and at the same time remain as compatible as possible with the primary documentation (the PtFEM book), I will store these Julia versions in the "n Experiments"

Fundamental and great development work related to solving partial differential equations is done in several other Julia packages, e.g. [ApproxFun.jl](https://github.com/JuliaApproximation/ApproxFun.jl), [DifferentialEquations.jl](https://github.com/JuliaDiffEq/DifferentialEquations.jl), [JuliaFEM.jl](http://www.juliafem.org) and, as mentioned earlier,  [JuaFEM.jl](https://github.com/KristofferC/JuAFEM.jl) to name a few.


Please note that no timeline is set when this work in progress will be finished.

Rob J Goedman
January 2017

References:

1. [Programming the Finite Element Method](http://www.wiley.com/WileyCDA/WileyTitle/productCd-1119973341.html)
1. [CSoM.jl](https://github.com/goedman/CSoM.jl)
1. [deal.II](http://dealii.org)
1. [FEniCS](https://fenicsproject.org)
1. [JuaFEM.jl](https://github.com/KristofferC/JuAFEM.jl)
1. [Julia language](http://julialang.org)
1. [ApproxFun.jl](https://github.com/JuliaApproximation/ApproxFun.jl)
1. [DifferentialEquations.jl](https://github.com/JuliaDiffEq/DifferentialEquations.jl)
1. [JuliaFEM.jl](http://www.juliafem.org)
1. [Numerical Methods for Engineer](https://books.google.com/books?id=lxGPQmuSwBQC&source=gbs_similarbooks)
1. [NMfE.jl](https://github.com/goedman/NMfE.jl)
1. [Symata.jl](https://github.com/jlapeyre/Symata.jl)