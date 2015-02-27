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

include(Pkg.dir("CSoM", "src", "CSoM", "elements.jl"))
include(Pkg.dir("CSoM", "src", "CSoM", "FEM.jl"))
include(Pkg.dir("CSoM", "src", "CSoM", "FEmodel.jl"))
include(Pkg.dir("CSoM", "src", "CSoM", "formnf.jl"))
include(Pkg.dir("CSoM", "src", "CSoM", "num_to_g.jl"))
include(Pkg.dir("CSoM", "src", "CSoM", "fkdiag.jl"))
include(Pkg.dir("CSoM", "src", "CSoM", "fsparv.jl"))
include(Pkg.dir("CSoM", "src", "CSoM", "spabac.jl"))
include(Pkg.dir("CSoM", "src", "CSoM", "sparin.jl"))
include(Pkg.dir("CSoM", "src", "CSoM", "rigid_jointed.jl"))
include(Pkg.dir("CSoM", "src", "CSoM", "pin_jointed.jl"))
include(Pkg.dir("CSoM", "src", "CSoM", "mesh_size.jl"))
include(Pkg.dir("CSoM", "src", "CSoM", "geom_rect.jl"))
include(Pkg.dir("CSoM", "src", "CSoM", "sample.jl"))
include(Pkg.dir("CSoM", "src", "CSoM", "deemat.jl"))
include(Pkg.dir("CSoM", "src", "CSoM", "beemat.jl"))
include(Pkg.dir("CSoM", "src", "CSoM", "shape_fun.jl"))
include(Pkg.dir("CSoM", "src", "CSoM", "shape_der.jl"))
include(Pkg.dir("CSoM", "src", "CSoM", "rod_km.jl"))
include(Pkg.dir("CSoM", "src", "CSoM", "rod_mm.jl"))
include(Pkg.dir("CSoM", "src", "CSoM", "beam_km.jl"))
include(Pkg.dir("CSoM", "src", "CSoM", "beam_mm.jl"))
include(Pkg.dir("CSoM", "src", "CSoM", "global_to_axial.jl"))
include(Pkg.dir("CSoM", "src", "CSoM", "hinge.jl"))
include(Pkg.dir("CSoM", "src", "CSoM", "glob_to_loc.jl"))
include(Pkg.dir("CSoM", "src", "CSoM", "loc_to_glob.jl"))
include(Pkg.dir("CSoM", "src", "CSoM", "checon.jl"))
include(Pkg.dir("CSoM", "src", "CSoM", "linmul_sky.jl"))
include(Pkg.dir("CSoM", "src", "CSoM", "stability.jl"))
include(Pkg.dir("CSoM", "src", "CSoM", "beam_gm.jl"))
include(Pkg.dir("CSoM", "src", "NMfE", "lufac.jl"))
include(Pkg.dir("CSoM", "src", "NMfE", "ldlt.jl"))
include(Pkg.dir("CSoM", "src", "NMfE", "ivp.jl"))
include(Pkg.dir("CSoM", "src", "NMfE", "bvp.jl"))

### Exports ###

export
  # From elements.jl
  ElementType,
  Plane,
  Beam,
  Rod,
  Frame,
  
  Element,
  Line,
  Triangle,
  Quadrilateral,
  Hexahedron,
  
  # From FEM.jl
  FEM,
  
  # From FEmodel.jl
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
  pin_jointed!,
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
  rod_mm!,
  beam_km!,
  beam_mm!,
  global_to_axial,
  hinge!,
  loc_to_glob!,
  glob_to_loc!,
  checon!,
  linmul_sky!,
  stability!,
  beam_gm!,
  
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
