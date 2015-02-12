module CSoM

if VERSION.minor < 4
  using Docile, Lexicon
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

include("FEM.jl")
include("FEbeam.jl")
include("FEmodel.jl")
include("FE4_1.jl")
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
include(Pkg.dir("CSoM", "src", "CSoM", "rod_km.jl"))

### Exports ###

export
  # From FEM.jl
  ElementType,
  Plane,
  Beam,
  Rod,
  
  Element,
  Line,
  Axisymmetric,
  Triangle,
  Quadrilateral,
  Hexahedron,
  
  FEM,
  
  # From FEmodel.jl
  FEmodel,
  
  # From FEbeam.jl
  FEbeam,
  
  # From FE4_1.jl
  FE4_1,
  
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
  rod_km!,
  
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
