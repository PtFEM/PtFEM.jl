# CSoM


[![Build Status](https://travis-ci.org/goedman/CSoM.jl.svg?branch=master)](https://travis-ci.org/goedman/CSoM.jl)


##Access to CSoM models

These are (some of) the programs as described in "Programming the Finite Element Method" by I M Smith and D V Griffiths. The package should probably be called PtFEM, but for now I have always related them to Colorado School of Mines (being a mining engineer from Delft University myself).

Initial focus is on chapters 4 to 6 to figure out how to structure the programs. For inspiration I'm looking of course at the PtFEM book but also a bit at deal.II and (to a lesser extent) FEniCS.

For now 'basic' functions are in the src/CSoM directory, computational flow 'templates' are in src/Chapxx directories. The idea of the templates is that they can be used to quickly set up similar models, e.g. 3D beams.

Once the structure is decided the basic functions will be reviewed for better approaches in Julia vs. the current Fortran flavor.

Currently mostly being tested on Julia v0.5. Some testing is happening on 0.6

No timeline is set when this work in progress will be finished.

Lots of other great stuff related to solving partial differentiel equiations is done in Julia packages, e.g. ApproxFun.jl, DifferentialEquations.jl, JuliaFem.jl, JuaFEM.jl to name a few.

Also note the NMfE.jl package, which contains the programs in the book "Numerical Methods for Engineers" by the same authors. In particular, Chapter 7 contains some Symata.jl based examples of applying the method of weighted residuals (MRW).

Rob J Goedman
January 2017

References:

1. [Programming the Finite Element Method](http://www.wiley.com/WileyCDA/WileyTitle/productCd-1119973341.html)
2. [Numerical Methods for Engineer](https://books.google.com/books?id=lxGPQmuSwBQC&source=gbs_similarbooks)
3. [FEniCS](https://fenicsproject.org)
4. [deal.II](http://dealii.org)
5. [ApproxFun.jl](https://github.com/JuliaApproximation/ApproxFun.jl)
6. [DifferentialEquations.jl](https://github.com/JuliaDiffEq/DifferentialEquations.jl)
7. [JuliaFEM.jl](http://www.juliafem.org)
8. [JuaFEM.jl](https://github.com/KristofferC/JuAFEM.jl)
9. [Julia language](http://julialang.org)