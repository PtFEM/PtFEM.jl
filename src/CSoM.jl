module CSoM

if VERSION.minor < 4
  using Docile, Lexicon
  macro doc_mstr(text)
     Base.triplequoted(text)
  end
  macro doc_str(text)
     text
  end
  
  export
    # From CSoM
    @doc_mstr,
    @doc_str
  
end
       
# package code goes here
### Imports ###

### Includes ###

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

# Only needed for comparing with Fortran performance
if isdir(Pkg.dir("CSoM", "deps"))
  include("csomif/if.jl")
  include("FEmodelFortran.jl")
end

### Exports ###

export
  # From FEmodel.jl
  FEmodel,
  # From FEmodelFortran.jl
  FEmodelFortran,
  
  # From CSoM
  formnf!,
  num_to_g!,
  fkdiag!,
  fsparv!,
  spabac!,
  sparin!,
  rigid_jointed!,
  
  # From NMfE
  lufac,
  ldlt

### Deprecated ###
  
  include("deprecated.jl")

end # module
