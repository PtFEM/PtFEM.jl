module CSoM

using Docile, Lexicon
@document

if VERSION.minor < 4
  #=
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
  =#
end

if !isdefined(Main, :JULIA_SVG_BROWSER)
  JULIA_SVG_BROWSER = ""
  try
    JULIA_SVG_BROWSER = ENV["JULIA_SVG_BROWSER"]
  catch e
    println("Environment variable JULIA_SVG_BROWSER not found.")
    JULIA_SVG_BROWSER = ""
  end
end

export
  JULIA_SVG_BROWSER
       
# package code goes here
### Imports ###

### Includes ###

include("FEmodel.jl")
include(Pkg.dir("CSoM", "src", "NMfE", "lufac.jl"))
include(Pkg.dir("CSoM", "src", "NMfE", "ldlt.jl"))
include(Pkg.dir("CSoM", "src", "NMfE", "ivp.jl"))
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
  fsparv,
  spabac!,
  sparin!,
  rigid_jointed!,
  f, f1, f2,
  
  # From NMfE
  lufac,
  ldlt,
  euler,
  modified_euler,
  mid_point_euler,
  runga_kutta_4

### Deprecated ###
  
  include("deprecated.jl")

end # module
