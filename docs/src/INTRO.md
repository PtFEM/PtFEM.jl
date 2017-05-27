# Introduction

## PtFEM toolkit

This Julia package will contain the programs in "Programming the Finite Element Method" by I M Smith, D V Griffiths and L. Margetts ([PtFEM](http://wiley.com/WileyCDA/WileyTitle/productCd-1119973341.html )). See [TODO](https://github.com/PtFEM/PtFEM.jl/blob/master/docs/src/TODO.md) for the planned progress.

PtFEM is a very versatile toolkit to construct FEM programs for practical engineering and scientific problems (to distinguish it somewhat from systems primarily focused on solving symbolic partial differential equations). Each chapter in the book gradually develops a set of related programs intended to be used as a starting point for a particular class of problems.

I use PtFEM when referring to the book and PtFEM.jl when referring to the Julia package. 

## A Julia based PtFEM eco system

[PtFEM.jl](https://github.com/PtFEM/PtFEM.jl) is the central package in a mini [PtFEM](https://github.com/PtFEM) eco system and most other packages in that eco system will be using PtFEM.jl as the starting point.

For many years a colleague and I have used the Fortran version of the PtFEM toolkit  to verify aspects of a (larger) software program developed to analyse the behavior of bottom hole assemblies (BHAs). PtFEM has proven extremely valuable for that purpose and in my opinion that alone justifies the creation of PtFEM.jl.

We are now at a point where we would like to publish the results of our work in a reproducable (and maintainable) format. Thus an important secundairy motivation for creating PtFEM.jl is to be able to subsequently publish 2 additional packages, [BHATheoreticalPerformance.jl](https://github.com/PtFEM/BHATheoreticalPerformance.jl) and [BHALockup.jl](https://github.com/PtFEM/BHALockup.jl), on the basis of a well documented toolkit.

The goal of the PtFEM eco system is to not only make our results easily reproducable but also to potentially capture broader usage of PtFEM, e.g. see the examples towards the end of  [TODO](https://github.com/PtFEM/PtFEM.jl/blob/master/docs/src/TODO.md). If there is long term interest to contribute to the PtFEM organisation, please consider becoming a team member or outside collaborator.

The PtFEM eco system exists as a Github organization. A GitHub organisation is basically a place to collect a set of related packages, in this case Julia packages around the "Programming the Finite Element Method" toolkit.

Occasionally a package might be temporarily selfstanding, e.g. [ClassicalLaminateTheory.jl](https://github.com/PtFEM/ClassicalLaminateTheory.jl) at present. The intention is to in the future upgrade/extend/incorporate that package, e.g. in the case of ClassicalLaminateTheory.jl to handle composite Bottom Hole Assembly modeling or extended reach casing installation.

A companion package to PtFEM, [NMfE.jl](https://github.com/PtFEM/NMfE.jl) along the lines of [Numerical Methods for Engineers](https://www.crcpress.com/Numerical-Methods-for-Engineers-Second-Edition/Griffiths-Smith/p/book/9781584884019) by I M Smith and D V Griffiths, is also included in the PtFEM organisation but development work for this educational set of programs is happening on a slower pace because:
1. In my mind it should form a bridge to some of the Julia packages mentioned in the [README](https://github.com/PtFEM/PtFEM.jl/blob/master/README.md) under related packages and focused on solving differential equations
2. It is less directly derived from the original Fortran programs, e.g. the symbolic examples in chapter 7 of NMfE using the Julia [Symata](https://github.com/jlapeyre/Symata.jl) package.

