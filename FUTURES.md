## The future of the GitHub PtFEM organisation

A GitHub organisation is basically a place to collect a set of related packages, in this case Julia packages around the "Programming the Finite Element Method" toolkit.

[PtFEM.jl](https://github.com/goedman/PtFEM.jl) is the central package in this mini-[PtFEM](https://github.com/PtFEM)-ecosystem  and most other packages will be using PtFEM.jl as the starting point.

My primary interest in publishing PtFEM.jl is to subsequently publish 2 additional packages, [BHATheoreticalPerformance.jl](https://github.com/PtFEM/BHATheoreticalPerformance.jl) and [BHALockup.jl](https://github.com/PtFEM/BHALockup.jl), on the basis of PtFEM.jl.

If there is long term interest to contribute to the PtFEM organisation, e.g. application of PtFEM.jl to reservoir engineering, please consider becoming a team member or outside collaborator.

Occasionally a package might be temporarily selfstanding, e.g. ClassicalLaminateTheory.jl at present. The intention is to in the future upgrade/extend/incorporate that package to handle composite Bottom Hole Assembly modeling or extended reach casing installation.

A companion package to PtFEM, [NMfE.jl](https://github.com/goedman/NMfE.jl) along the lines of [Numerical Methods for Engineers](https://www.crcpress.com/Numerical-Methods-for-Engineers-Second-Edition/Griffiths-Smith/p/book/9781584884019) by I M Smith and D V Griffiths, is also included in the PtFEM organisation but development work for this educational set of programs is happening on a slower pace because:
1. In my mind it should form a bridge to some of the  Julia packages mentioned in the [README](https://github.com/PtFEM/PtFEM.jl/blob/master/README.md) under related packages and focused to solving differential equations
2. It is less directly derived from the original Fortran programs, e.g. the symbolic examples in chapter 7 of NMfE using the Julia [Symata](https://github.com/jlapeyre/Symata.jl) package.

