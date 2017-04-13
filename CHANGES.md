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
