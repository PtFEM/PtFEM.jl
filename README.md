# PtFEM


[![Project Status: WIP - Initial development is in progress, but there has not yet been a stable, usable release suitable for the public.](http://www.repostatus.org/badges/latest/wip.svg)](http://www.repostatus.org/#wip)

[![Travis Build Status](https://travis-ci.org/PtFEM/PtFEM.jl.svg?branch=master)](https://travis-ci.org/PtFEM/PtFEM.jl)


## Access to PtFEM models

This Julia package contains the programs in chapters 4 to 6 as described in "Programming the Finite Element Method" by I M Smith, D V Griffiths and L. Margetts ([PtFEM]( http://www.wiley.com/WileyCDA/WileyTitle/productCd-1119973341.html )).

I use PtFEM when referring to the book and PtFEM.jl when referring to the Julia package.

PtFEM.jl is the central package in the Github PtFEM organisation. More details about the plans for the PtFEM organisation are given below in the "Futures" section.

PtFEM.jl will be published in 3 steps, 1) chapters 4-6, 2) chapters 7-9 and 3) remaining chapters in PtFEM.

To use the toolkit and run several test programs, start the [Julia](http://julialang.org) REPL and type:

```
Pkg.clone("https://github.com/PtFEM/PtFEM.jl")
Pkg.test("PtFEM")
```

As this shows, the package has not yet been published and is currently only available through cloning from my github account. The authors have given permission to publish the Julia version of the PtFEM toolkit. Please refer to the [LICENSE](https://github.com/goedman/PtFEM.jl/blob/master/LICENSE.md) file for more details.

Examples are in the examples/... and notebooks/... subdirectories. Currently the notebooks are just initial examples (and might not always work) until the structure of the toolkit gels.

Initial focus has been on chapters 4 and 5 of PtFEM in order to figure out how to best structure the toolkit. At this point chapter 4 is mostly done.

Please note that no timeline is set when this work in progress will be finished.

Rob J Goedman
March 2017

## Changes with respect to the PtFEM book

The programs are being reviewed for better approaches in [Julia](http://julialang.org) vs. the current 'translated-from-Fortran' flavor. 

Examples of these kind of changes are to drop skyline storage and directly using Julia sparse matrices and replacing PtFEM's pair sparin() and spabac() by Julia's cholfact() and "\\" operator.

E.g. an example of this last change is replacing

```
  PtFEM.sparin!(kv, kdiag)
  loads[2:end] = PtFEM.spabac!(kv, loads[2:end], kdiag)
```

by

```
  # Cholesky decomposed global stiffness matrix
  cfgsm = cholfact(gsm)
  loads[2:end] = cfgsm \ loads[2:end]
```

All 'basic' functions such as sparin!() and spabac!() can be found in the src/PtFEM directory. Note the use of the "!" in the function name which is the Julia convention for functions that update one or more of the function arguments. If specific Julia versions are required for use in the templates, these are added to the respective source files. Often times Julia's multiple dispatching takes care of selecting the correct version in the templates.

## Related work

Fundamental and great development work related to solving (partial) differential equations is done in several other Julia packages, e.g. [ApproxFun.jl](https://github.com/JuliaApproximation/ApproxFun.jl), [DifferentialEquations.jl](https://github.com/JuliaDiffEq/DifferentialEquations.jl), [JuliaFEM.jl](http://www.juliafem.org) and  [JuaFEM.jl](https://github.com/KristofferC/JuAFEM.jl) to name a few.

Outside of Julia at least 2 other toolkits should be mentioned, i.e.  [deal.II](http://dealii.org) and [FEniCS](https://fenicsproject.org).

## The future of the GitHub PtFEM organisation

A GitHub organisation is basically a place to collect a set of related packages, in this case Julia packages around the "Prohramming the Finite Element Method" toolkit.

As stated above, PtFEM.jl is the central package in this mini-PtFEM-ecosystem and most other packages will be using PtFEM.jl as the starting point. 

Occasionally a package might be temporarily selfstanding, e.g. ClassicalLaminateTheory.jl at present. The intention is to in the future upgrade/extend/incorporate that package to handle composite Bottom Hole Assembly modeling or extended reach casing installation.

A companion package to [PtFEM.jl](https://github.com/goedman/PtFEM.jl), [NMfE.jl](https://github.com/goedman/NMfE.jl) along the lines of [Numerical Methods for Engineers](https://www.crcpress.com/Numerical-Methods-for-Engineers-Second-Edition/Griffiths-Smith/p/book/9781584884019) by I M Smith and D V Griffiths, is also included in the PtFEM organisation but development work for this educational set of programs is happening on a slower pace because:
1. In my mind it should form a bridge to some of the above mentioned Julia packages related to solving differential equations
2. It is less directly derived from the original Fortran programs, e.g. the symbolic examples in chapter 7 of NMfE using the Julia Symata package.

Over time several more packages are envisaged to be added to PtFEM.

## References

1. [Programming the Finite Element Method](http://www.wiley.com/WileyCDA/WileyTitle/productCd-1119973341.html)
1. [PtFEM.jl](https://github.com/goedman/PtFEM.jl)
1. [deal.II](http://dealii.org)
1. [FEniCS](https://fenicsproject.org)
1. [JuaFEM.jl](https://github.com/KristofferC/JuAFEM.jl)
1. [Julia language](http://julialang.org)
1. [ApproxFun.jl](https://github.com/JuliaApproximation/ApproxFun.jl)
1. [DifferentialEquations.jl](https://github.com/JuliaDiffEq/DifferentialEquations.jl)
1. [JuliaFEM.jl](http://www.juliafem.org)
1. [Numerical Methods for Engineers](https://www.crcpress.com/Numerical-Methods-for-Engineers-Second-Edition/Griffiths-Smith/p/book/9781584884019)
1. [NMfE.jl](https://github.com/goedman/NMfE.jl)
1. [Symata.jl](https://github.com/jlapeyre/Symata.jl)