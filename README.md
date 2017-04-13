# PtFEM


[![Project Status: WIP - Initial development is in progress, but there has not yet been a stable, usable release suitable for the public.](http://www.repostatus.org/badges/latest/wip.svg)](http://www.repostatus.org/#wip)

[![Travis Build Status](https://travis-ci.org/PtFEM/PtFEM.jl.svg?branch=master)](https://travis-ci.org/PtFEM/PtFEM.jl)


## Access to PtFEM models

This Julia package contains the programs in chapters 4 to 6 as described in "Programming the Finite Element Method" by I M Smith, D V Griffiths and L. Margetts ([PtFEM]( http://www.wiley.com/WileyCDA/WileyTitle/productCd-1119973341.html )).

I use PtFEM when referring to the book and PtFEM.jl when referring to the Julia package.

PtFEM.jl is the central package in the Github [PtFEM organisation](https://github.com/PtFEM). More details about the plans for the PtFEM organisation are given in the FUTURES.md file.

Changes in PtFEM.jl with respect to the Fortran programs in PtFEM are summarized in the CHANGES.md file.

PtFEM.jl will be published in a couple of steps, e.g. 1) chapters 4-6, 2) chapters 7-9, 3) remaining chapters in PtFEM and 4) VTK output and multiprocessing aspects, although I might start to mix in item 4) earlier. Several examples use [Plots.jl](https://juliaplots.github.io) based graphics.

To use the toolkit and run the test programs, start the [Julia](http://julialang.org) REPL and type:

```
Pkg.clone("https://github.com/PtFEM/PtFEM.jl")
Pkg.test("PtFEM")
```

As this shows, the package has not yet been published and is currently only available through cloning from my github account. The authors have given permission to publish the Julia version of the PtFEM toolkit. Please refer to the [LICENSE](https://github.com/goedman/PtFEM.jl/blob/master/LICENSE.md) file for more details.

Examples are in the examples/...  subdirectories. 

I also have some [notebooks](https://github.com/goedman/RobGoedmansNotebooks.jl/tree/master/notebooks/PtFEM). At this point in time, the notebooks are just initial examples (and might not always work) until the structure of the toolkit gels.

Initial focus has been on chapters 4, 5 and early sections of 6 of PtFEM in order to figure out how to best structure the toolkit. At this point chapter 4 is mostly done, chapters 5 and 6 are being reviewed.

Please note that no timeline is set when this work in progress will be finished.

Rob J Goedman
March 2017

## Related work

Fundamental and great development work related to solving (partial) differential equations is done in several other Julia packages, e.g. [ApproxFun.jl](https://github.com/JuliaApproximation/ApproxFun.jl), [DifferentialEquations.jl](https://github.com/JuliaDiffEq/DifferentialEquations.jl), [JuliaFEM.jl](http://www.juliafem.org) and  [JuaFEM.jl](https://github.com/KristofferC/JuAFEM.jl) to name a few.

Outside of Julia at least 2 other toolkits should be mentioned, i.e.  [deal.II](http://dealii.org) and [FEniCS](https://fenicsproject.org).

References will be kept in the REFERENCES.md file.

