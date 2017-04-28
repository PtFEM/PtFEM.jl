# Changes with respect to the PtFEM book


The PtFEM book is the primary source to understand how the Fortran toolkit can be used to build FEM programs. This is also true for the Julia version of the PtFEM toolkiit, PtFEM.jl.

But even with this restriction in place, there are many ways to port the PtFEM toolkit to Julia. Julia can in fact call the lower level Fortran "building blocks" (subroutines) directly. But that would make it harder to modify those functions.
PtFEM.jl is entirely written in Julia end takes a middle of the road approach in replacing Fortran functionality by "typical" Julia features. These cases are documented in this file.

If additional Julia versions of functions, particularly "building blocks", are required for use in the programs, these are added to the respective source files. Often times Julia's "multiple dispatch" takes care of selecting the correct version in the templates.

## Julia's convention for functions that update arguments

Note the use of the "!" in some function names which is the Julia convention for functions that update one or more of the function arguments. 

## Custom array indices

Julia by default uses 1 as the first index into an array, but has the ability to use arbitrary indexing as well. The PtFEM Fortran programs use 0-based indexing for the loads vector. In programs p41 through to p44 in chapter 4 I have used OffsetArrays.jl for this purpose, i.e:

```
using OffsetArrays
N = 10
loads = OffsetArray(zeros(N+1), 0:N)
```

I'm planning to use the same approach in all other chapters.

## Replacing skyline storage by Julia sparse matrices

In the programs for chapter 4, the skyline storage idea has been replaced by Julia sparse matrices and, accordingly, PtFEM's pair sparin() and spabac() by Julia's cholfact() and "\\" operator.

Thus

```
  PtFEM.sparin!(kv, kdiag)
  loads[2:end] = PtFEM.spabac!(kv, loads[2:end], kdiag)
```

has been replaced by

```
  # Cholesky decomposed global stiffness matrix
  cfgsm = cholfact(gsm)
  loads[2:end] = cfgsm \ loads[2:end]
```

All 'basic' functions such as sparin!() and spabac!() can be found in the src/PtFEM directory.

## Separate equivalent loads  in data dictionary

E.g. p44. 

In p44 corections are applied if :eq_nodal_forces_and_moments is defined in the data dictionary.

See [PtFEM/EEM.jl](https://github.com/PtFEM/EEM.jl) for further examples.

## Graphics

Graphics will be mostly implemented using the Julia pacckage Plots.jl (using the GR.jl backend).

#### Plots.jl

E.g. Ex41.1.jl, Ex61.1.jl and Ex62.1.jl

Several programs will generate VTK output.

#### VTK (ParaView)

E.g. Ex47.1.jl

## Initial introduction of parallel programming in Julia

Some examples will show simple ways of using Julia's capabilities in this area.

In Chapter 6, example Exp62.1a.jl calls p62a.jl which uses Julia pmap() for this purpose. This is too small a problem to really show performance improvements, but it shows an easy approach.

_Note 1_: In example Ex62.1a.jl could have called the p62(data) as Ex62.1.jl does; pp62(data) is identical but produces less output.

_Note 2_: I have not done a profiling pass through p62.jl, it allocates way too much memory, so I expect significant performance improvements are possible.

## Integration - for now using PtFEM's approach

Currently I have not replaced numerical integration by e.g. Julia quadgk() for 1D integration of a function.