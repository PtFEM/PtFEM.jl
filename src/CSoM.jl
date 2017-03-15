module CSoM

using Compat

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
include("CSoM/finite_elements.jl")
include("CSoM/structural_elements.jl")
include("CSoM/FEM.jl")
include("CSoM/FEmodel.jl")
include("CSoM/formnf.jl")
include("CSoM/num_to_g.jl")
include("CSoM/fkdiag.jl")
include("CSoM/fsparv.jl")
include("CSoM/spabac.jl")
include("CSoM/sparin.jl")
include("CSoM/rigid_jointed.jl")
include("CSoM/pin_jointed.jl")
include("CSoM/mesh_size.jl")
include("CSoM/geom_rect_triangle.jl")
include("CSoM/geom_rect_quadrilateral.jl")
include("CSoM/hexahedron_xz.jl")
include("CSoM/sample_hexahedron.jl")
include("CSoM/sample_line.jl")
include("CSoM/sample_quadrilateral.jl")
include("CSoM/sample_tetrahedron.jl")
include("CSoM/sample_triangle.jl")
include("CSoM/deemat.jl")
include("CSoM/beemat_nonaxi.jl")
include("CSoM/beemat.jl")
include("CSoM/shape_fun.jl")
include("CSoM/shape_der.jl")
include("CSoM/rod_km.jl")
include("CSoM/rod_mm.jl")
include("CSoM/beam_km.jl")
include("CSoM/beam_mm.jl")
include("CSoM/global_to_axial.jl")
include("CSoM/hinge.jl")
include("CSoM/glob_to_loc.jl")
include("CSoM/loc_to_glob.jl")
include("CSoM/checon.jl")
include("CSoM/linmul_sky.jl")
include("CSoM/stability.jl")
include("CSoM/beam_gm.jl")
include("CSoM/fmplat.jl")
include("CSoM/read_generated_data.jl")
include("CSoM/invar.jl")
include("CSoM/formm.jl")
include("CSoM/exportVTK_XML.jl")
include("CSoM/fromSkyline.jl")
include("CSoM/skyline2sparse.jl")

include("4 Static Equilibrium/FE4_1.jl")
include("4 Static Equilibrium/FE4_2.jl")
include("4 Static Equilibrium/FE4_3.jl")
include("4 Static Equilibrium/FE4_4.jl")
include("4 Static Equilibrium/FE4_5.jl")
include("4 Static Equilibrium/FE4_6.jl")
include("4 Static Equilibrium/FE4_7.jl")

include("5 Elastic Solids/FE5_1.jl")
include("5 Elastic Solids/FE5_2.jl")
include("5 Elastic Solids/FE5_3.jl")
include("5 Elastic Solids/FE5_4.jl")
include("5 Elastic Solids/FE5_5.jl")
include("5 Elastic Solids/FE5_6.jl")

include("6 Material Nonlinearity/FE6_1.jl")

### Exports ###

export
  # From fin_els.jl
  StructuralElement,
  Rod,
  Beam,
  Frame,
  Plane,
  Solid,
  GenericSolid,
  
  FiniteElement,
  Line,
  Triangle,
  Quadrilateral,
  Hexahedron,
  Tetrahedron,
  
  # From FEM.jl
  FEM,
  
  # From FEmodel.jl
  FEmodel,
  
  write_VTKXML,
  VTKNode,
  VTKElement,
  fromSkyline,
  skyline2sparse,
  
  # From Chap04
  FE4_1,
  FE4_2,
  FE4_3,
  FE4_4,
  FE4_5,
  FE4_6,
  FE4_7,
  
  # From Chap05
  FE5_1,
  FE5_2,
  FE5_3,
  FE5_4,
  FE5_5,
  FE5_6,
  
  # From Chap05
  FE6_1
  
### Deprecated ###
  
  include("deprecated.jl")

end # module
