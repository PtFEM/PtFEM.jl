module CSoM

# package code goes here
### Imports ###

### Includes ###

include("csomif/if.jl")
include("FEmodel.jl")
include(Pkg.dir("CSoM", "src", "NMfE", "lufac.jl"))
include(Pkg.dir("CSoM", "src", "NMfE", "ldlt.jl"))

### Exports ###

export
  # From FEmodel.jl
  FEmodel,
  
  # From lufac.jl
  lufac,
  
  # From ldlt.jl
  ldlt

### Deprecated ###
  
  include("deprecated.jl")

end # module
