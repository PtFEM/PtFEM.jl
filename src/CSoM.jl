module CSoM

#=
using Docile, Lexicon
@document
=#

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

include("FEbeam.jl")
include("FEM.jl")
include(Pkg.dir("CSoM", "src", "NMfE", "lufac.jl"))
include(Pkg.dir("CSoM", "src", "NMfE", "ldlt.jl"))
include(Pkg.dir("CSoM", "src", "NMfE", "ivp.jl"))
include(Pkg.dir("CSoM", "src", "NMfE", "bvp.jl"))
include(Pkg.dir("CSoM", "src", "CSoM", "formnf.jl"))
include(Pkg.dir("CSoM", "src", "CSoM", "num_to_g.jl"))
include(Pkg.dir("CSoM", "src", "CSoM", "fkdiag.jl"))
include(Pkg.dir("CSoM", "src", "CSoM", "fsparv.jl"))
include(Pkg.dir("CSoM", "src", "CSoM", "spabac.jl"))
include(Pkg.dir("CSoM", "src", "CSoM", "sparin.jl"))
include(Pkg.dir("CSoM", "src", "CSoM", "rigid_jointed.jl"))
include(Pkg.dir("CSoM", "src", "CSoM", "mesh_size.jl"))
include(Pkg.dir("CSoM", "src", "CSoM", "geom_rect.jl"))
include(Pkg.dir("CSoM", "src", "CSoM", "sample.jl"))
include(Pkg.dir("CSoM", "src", "CSoM", "deemat.jl"))
include(Pkg.dir("CSoM", "src", "CSoM", "beemat.jl"))
include(Pkg.dir("CSoM", "src", "CSoM", "shape_fun.jl"))
include(Pkg.dir("CSoM", "src", "CSoM", "shape_der.jl"))

# Only needed for comparing with Fortran performance
if isfile(Pkg.dir("CSoM", "deps", "src", "CSoM", "4th_ed", "libcsom.dylib"))
  include("csomif/if.jl")
  include("FEbeamFortran.jl")
  export
    FEbeamFortran
end

### Exports ###

export
  # From FEbeam.jl
  FEbeam,
  
  # From FEM.jl
  Element,
  Plane,
  
  Element,
  Axisymmetric,
  Triangle,
  Quadrilateral,
  Hexahedron,
  
  FE,
  FEmodel,
  
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
  shootingmethod,
  mesh_size,
  geom_rect!,
  sample!,
  deemat!,
  beemat!,
  shape_fun!,
  shape_der!,
  
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
