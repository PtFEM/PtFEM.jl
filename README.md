# CSoM


[![Build Status](https://travis-ci.org/goedman/CSoM.jl.svg?branch=master)](https://travis-ci.org/goedman/CSoM.jl)


##Access to CSoM models

These are (some of) the programs as described in "Programming the Finite Element Method" by I M Smith and D V Griffiths. The package should probably be called PtFEM.jl, but for now I have always related them to Colorado School of Mines (being a mining engineer from Delft University myself), hence [CSoM.jl](https://github.com/goedman/CSoM.jl).

Initial focus is on chapters 4 to 6 in order to figure out how to best structure the programs. For inspiration I'm looking of course at the PtFEM book but also a bit at [deal.II](http://dealii.org) and (to a lesser extent) [FEniCS](https://fenicsproject.org). I'm also looking to possibly reuse some of the approaches/features in [JuaFEM](http://kristofferc.github.io/JuAFEM.jl/latest/).

All 'basic' functions are in the src/CSoM directory while higher level computational flow 'templates' are in src/Chapxx directories. The idea of the templates is that they can be used to quickly set up similar models, e.g. 3D beams (see the Beams examples in [CSoM.jl](https://github.com/goedman/CSoM.jl/tree/master/examples/Beams)).

Once the structure has been decided upon the basic functions will be reviewed for better approaches in [Julia](http://julialang.org) vs. the current Fortran flavor. 

Lots of other great development work related to solving partial differentiel equiations is done in Julia packages, e.g. [ApproxFun.jl](https://github.com/JuliaApproximation/ApproxFun.jl), [DifferentialEquations.jl](https://github.com/JuliaDiffEq/DifferentialEquations.jl), [JuliaFEM.jl](http://www.juliafem.org) and, as mentioned earlier,  [JuaFEM.jl](https://github.com/KristofferC/JuAFEM.jl) to name a few.

Also note the  [NMfE.jl](https://github.com/goedman/NMfE.jl) package, which contains the programs in the book "Numerical Methods for Engineers" by the same authors. NMfE is a great introduction to PtFEM. In particular, examples/ch07 directory of NMfE.jl contains some very interesting [Symata.jl](https://github.com/jlapeyre/Symata.jl) based introductory examples of applying the method of weighted residuals (MRW).

Currently CSoM.jl has switched Julia v0.6.

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