module CSoM

VERSION.minor < 4 && using Docile

# package code goes here
### Imports ###

### Includes ###

include("csomif/if.jl")
include("FEmodel.jl")
include(Pkg.dir("CSoM", "src", "NMfE", "lufac.jl"))
include(Pkg.dir("CSoM", "src", "NMfE", "ldlt.jl"))
include(Pkg.dir("CSoM", "src", "CSoM", "formnf.jl"))
include(Pkg.dir("CSoM", "src", "CSoM", "num_to_g.jl"))
include(Pkg.dir("CSoM", "src", "CSoM", "fkdiag.jl"))
include(Pkg.dir("CSoM", "src", "CSoM", "fsparv.jl"))
include(Pkg.dir("CSoM", "src", "CSoM", "spabac.jl"))
include(Pkg.dir("CSoM", "src", "CSoM", "sparin.jl"))
include(Pkg.dir("CSoM", "src", "CSoM", "rigid_jointed.jl"))

### Exports ###

export
  # From FEmodel.jl
  FEmodel,
  
  # From CSoM
  formnf,
  num_to_g,
  fkdiag,
  fsparv,
  spabac,
  sparin,
  rigid_jointed,
  
  # From NMfE
  lufac,
  ldlt

### Deprecated ###
  
  include("deprecated.jl")

end # module
