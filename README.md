# PtFEM

[![Project Status: WIP - Initial development is in progress, but there has not yet been a stable, usable release suitable for the public.](http://www.repostatus.org/badges/latest/wip.svg)](http://www.repostatus.org/#wip)

[![Coverage Status](https://coveralls.io/repos/PtFEM/PtFEM.jl/badge.svg?branch=master&service=github)](https://coveralls.io/github/PtFEM/PtFEM.jl?branch=master)
[![codecov](https://codecov.io/gh/PtFEM/PtFEM.jl/branch/master/graph/badge.svg)](https://codecov.io/gh/PtFEM/PtFEM.jl?branch=master)

Unix/OSX:  [![Travis Build Status](https://travis-ci.org/PtFEM/PtFEM.jl.svg?branch=master)](https://travis-ci.org/PtFEM/PtFEM.jl)

Windows(64bit):  [![Build status](https://ci.appveyor.com/api/projects/status/github/PtFEM/PtFEM.jl?branch=master)](https://ci.appveyor.com/project/goedman/ptfem-jl)

Documentation:
[![](https://img.shields.io/badge/docs-latest-blue.svg)](https://ptfem.github.io/PtFEM.jl/latest)


## The "Programming the Finite Element Method" toolkit

This Julia package currently contains the programs in chapters 4, 5 and early sections of 6 as described in "Programming the Finite Element Method" by I M Smith, D V Griffiths and L. Margetts ([PtFEM]( http://www.wiley.com/WileyCDA/WileyTitle/productCd-1119973341.html )).

I use PtFEM when referring to the book and PtFEM.jl when referring to the Julia package.  The authors and publisher have given permission to publish the Julia version of the PtFEM toolkit. Please refer to [LICENSE](https://github.com/PtFEM/PtFEM.jl/blob/master/LICENSE.md) for more details.

## Documentation

 PtFEM, the book, will always remain the primary documentation for this package. Additional programming documentation will be available through Julia's documenter package, e.g. in-line after installing the package:

```
use PtFEM
?StructuralElement
```

and full documentation can be found  [here](http://ptfem.github.io/PtFEM.jl/latest/INTRO.html).

## Timeline

Please note that no hard timeline is set when this work in progress will be finished. [TODO](https://github.com/PtFEM/PtFEM.jl/blob/master/docs/src/TODO.md) contains a list of next steps. [VERSIONS](https://github.com/PtFEM/PtFEM.jl/blob/master/docs/src/VERSIONS.md) holds the tagged version history.

## Related work

Fundamental and great development work related to solving (partial) differential equations is done in several other Julia packages, e.g. [ApproxFun.jl](https://github.com/JuliaApproximation/ApproxFun.jl), [DifferentialEquations.jl](https://github.com/JuliaDiffEq/DifferentialEquations.jl), [JuliaFEM.jl](http://www.juliafem.org) and  [JuaFEM.jl](https://github.com/KristofferC/JuAFEM.jl) to name a few.

Outside of Julia at least 2 other toolkits should be mentioned, i.e.  [deal.II](http://dealii.org) and [FEniCS](https://fenicsproject.org).

## Rerences

References will be kept [here](https://github.com/PtFEM/PtFEM.jl/blob/master/docs/src/REFERENCES.md).

## Participation and feedback

As always, feedback is welcome, please send me an email, file an issue on Github or generate a pull request (PR).

Rob J Goedman
July 2018
