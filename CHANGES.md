## Changes with respect to the PtFEM book

The programs are being reviewed for better approaches in [Julia](http://julialang.org) vs. the 'translated-from-Fortran' flavor.

If specific Julia versions are required for use in the templates, these are added to the respective source files. Often times Julia's multiple dispatching takes care of selecting the correct version in the templates.

### Julia's convention for functions that update arguments

Note the use of the "!" in some function names which is the Julia convention for functions that update one or more of the function arguments. 

### Replacing skyline storage by Julia sparse matrices

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

### Separate equivalent loads  in data dictionary

E.g. p44. 

In p44 corections are applied if :eq_nodal_forces_and_moments is defined in the data dictionary.

See [PtFEM/EEM.jl](https://github.com/PtFEM/EEM.jl) for further examples.

### Custom array indices

Julia by default uses 1 as the first index into an array, but has the ability to use arbitrary indexing as well. The PtFEM Fortran programs use 0-based indexing for the loads vector. For now I have shifted all indices by 1. This practice will be reviewed later on in the project. I'm leaning towards simply using OffsetArrays.jl for this purpose, i.e:

```
neq = 10
loads = OffsetArray(zeros(neq+1), 0:neq)
```

### Graphics

#### Plots.jl

E.g. Ex41.1.jl, Ex61.1.jl and Ex62.1.jl

#### VTK (ParaView)

E.g. Ex47.1.jl

### Initial introduction of parallel programming in Julia

Some examples will show simple ways of using Julia's capabilities in this area.

In Chapter 6, example Exp62.1a.jl calls p62a.jl which uses Julia pmap() for this purpose. This is too small a problem to really show performance improvements, but it shows an easy approach.

_Note 1_: In example Ex62.1a.jl could have called the p62(data) as Ex62.1.jl does; pp62(data) is identical but produces less output.

_Note 2_: I have not done a profiling pass through p62.jl, it allocates way too much memory, so I expect significant performance improvements are possible.