# PtFEM


[![Project Status: WIP - Initial development is in progress, but there has not yet been a stable, usable release suitable for the public.](http://www.repostatus.org/badges/latest/wip.svg)](http://www.repostatus.org/#wip)

[![Travis Build Status](https://travis-ci.org/PtFEM/PtFEM.jl.svg?branch=master)](https://travis-ci.org/PtFEM/PtFEM.jl)
[![Build status](https://ci.appveyor.com/api/projects/status/github/PtFEM/PtFEM.jl?branch=master)](https://ci.appveyor.com/project/ptfem/ptfem-jl/branch/master)



## Access to PtFEM models

This Julia package currently contains the programs in chapters 4, 5 and early sections of 6 as described in "Programming the Finite Element Method" by I M Smith, D V Griffiths and L. Margetts ([PtFEM]( http://www.wiley.com/WileyCDA/WileyTitle/productCd-1119973341.html )).

I use PtFEM when referring to the book and PtFEM.jl when referring to the Julia package.

PtFEM is a very versatile toolkit to numerically solve problems consisting of physical components using FEM methods (to distinguish it from systems primarily focused on solving symbolic partial differential equations).

PtFEM.jl is the central package in the Github [PtFEM organisation](https://github.com/PtFEM). More details about the plans for the PtFEM organisation are given in  [FUTURES](https://github.com/PtFEM/PtFEM.jl/blob/master/FUTURES.md).

Changes in PtFEM.jl with respect to the Fortran programs in PtFEM are summarized in [CHANGES](https://github.com/PtFEM/PtFEM.jl/blob/master/CHANGES.md).

PtFEM.jl will be published in a couple of steps, e.g. 1) chapters 4-6, 2) chapters 7-9, 3) remaining chapters in PtFEM and 4) VTK output and multiprocessing aspects, although I might start to mix in item 4) earlier.

To use the toolkit and run the test programs, start the [Julia](http://julialang.org) REPL and type:

```
Pkg.clone("https://github.com/PtFEM/PtFEM.jl")
Pkg.test("PtFEM")
```

As this shows, the package has not yet been published and is currently only available through cloning from my github account. The authors have given permission to publish the Julia version of the PtFEM toolkit. Please refer to [LICENSE](https://github.com/goedman/PtFEM.jl/blob/master/LICENSE.md) for more details.

Examples are in the examples/...  subdirectories. Please note that several examples use [Plots.jl](https://juliaplots.github.io) based graphics. I have not made Plots.jl REQUIREd for PtFEM.jl. In the test directory there is a [runexamples](https://github.com/PtFEM/PtFEM.jl/blob/master/test/runexamples.jl) script that will run all examples.

 PtFEM will always remain the primary documentation for this package. Additional programming documentation will be available through Julia's documentor package, e.g. after installing the package:
 
```
use PtFEM
?Line
```

I also have some [notebooks](https://github.com/goedman/RobGoedmansNotebooks.jl/tree/master/notebooks/PtFEM). At this point in time, the notebooks are just initial examples (and might not always work) until the structure of the toolkit gels.

Initial focus has been on chapters 4, 5 and early sections of 6 of PtFEM in order to figure out how to best structure the toolkit. At this point chapter 4 is mostly done, chapters 5 and 6 are being reviewed.

Please note that no timeline is set when this work in progress will be finished. [TODO](https://github.com/goedman/PtFEM.jl/blob/master/TODO.md) contains a list of next steps. [VERSIONS](https://github.com/goedman/PtFEM.jl/blob/master/VERSIONS.md) holds the version history.

Rob J Goedman
March 2017

## Related work

Fundamental and great development work related to solving (partial) differential equations is done in several other Julia packages, e.g. [ApproxFun.jl](https://github.com/JuliaApproximation/ApproxFun.jl), [DifferentialEquations.jl](https://github.com/JuliaDiffEq/DifferentialEquations.jl), [JuliaFEM.jl](http://www.juliafem.org) and  [JuaFEM.jl](https://github.com/KristofferC/JuAFEM.jl) to name a few.

Outside of Julia at least 2 other toolkits should be mentioned, i.e.  [deal.II](http://dealii.org) and [FEniCS](https://fenicsproject.org).

References will be kept in [REFERENCES](https://github.com/PtFEM/PtFEM.jl/blob/master/REFERENCES.md).

