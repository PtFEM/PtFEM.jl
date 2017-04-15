## Changes with respect to the PtFEM book

The programs are being reviewed for better approaches in [Julia](http://julialang.org) vs. the 'translated-from-Fortran' flavor.

If specific Julia versions are required for use in the templates, these are added to the respective source files. Often times Julia's multiple dispatching takes care of selecting the correct version in the templates.

### Julia's convention for functions that update arguments

Note the use of the "!" in some function names which is the Julia convention for functions that update one or more of the function arguments. 

### Replacing skyline storage by Julia sparse matrices

In the prorams for chapter 4, the skyline storage idea has been replaced by Julia sparse matrices and, accordingly, PtFEM's pair sparin() and spabac() by Julia's cholfact() and "\\" operator.

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

### Initial introduction of parallel programming in Julia

Some examples will show simple ways of using Julia's capabilities in this area.

In Chapter 6, example Exp62.1a.jl calls p62a.jl which uses Julia pmap() for this purpose. This is too small a problem to really show tremendous performance improvements, but it shows an easy approach.

_Note 1_: In example Ex62.1a.jl could have called the p62(data) as Ex62.1.jl does; pp62(data) is identical but produces less output.

_Note 2_: I have not done a profiling pass through p62.jl, it allocates way too much memory, so I expect performance improvements are possible.