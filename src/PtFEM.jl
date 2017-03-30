module PtFEM

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
include("PtFEM/finite_elements.jl")
include("PtFEM/structural_elements.jl")
include("PtFEM/FEM.jl")
include("PtFEM/FEmodel.jl")
include("PtFEM/formnf.jl")
include("PtFEM/num_to_g.jl")
include("PtFEM/fkdiag.jl")
include("PtFEM/fsparv.jl")
include("PtFEM/spabac.jl")
include("PtFEM/sparin.jl")
include("PtFEM/rigid_jointed.jl")
include("PtFEM/pin_jointed.jl")
include("PtFEM/mesh_size.jl")
include("PtFEM/geom_rect_triangle.jl")
include("PtFEM/geom_rect_quadrilateral.jl")
include("PtFEM/hexahedron_xz.jl")
include("PtFEM/sample_hexahedron.jl")
include("PtFEM/sample_line.jl")
include("PtFEM/sample_quadrilateral.jl")
include("PtFEM/sample_tetrahedron.jl")
include("PtFEM/sample_triangle.jl")
include("PtFEM/deemat.jl")
include("PtFEM/beemat_nonaxi.jl")
include("PtFEM/beemat.jl")
include("PtFEM/shape_fun.jl")
include("PtFEM/shape_der.jl")
include("PtFEM/rod_km.jl")
include("PtFEM/rod_mm.jl")
include("PtFEM/beam_km.jl")
include("PtFEM/beam_mm.jl")
include("PtFEM/global_to_axial.jl")
include("PtFEM/hinge.jl")
include("PtFEM/glob_to_loc.jl")
include("PtFEM/loc_to_glob.jl")
include("PtFEM/checon.jl")
include("PtFEM/linmul_sky.jl")
include("PtFEM/stability.jl")
include("PtFEM/beam_gm.jl")
include("PtFEM/fmplat.jl")
include("PtFEM/read_generated_data.jl")
include("PtFEM/invar.jl")
include("PtFEM/formm.jl")
include("PtFEM/exportVTK_XML.jl")
include("PtFEM/fromSkyline.jl")
include("PtFEM/skyline2sparse.jl")

include("4 Static Equilibrium/p4_1.jl")
include("4 Static Equilibrium/p4_2.jl")
include("4 Static Equilibrium/p4_3.jl")
include("4 Static Equilibrium/p4_4.jl")
include("4 Static Equilibrium/p4_5.jl")
include("4 Static Equilibrium/p4_6.jl")
include("4 Static Equilibrium/p4_7.jl")

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
  p4_1,
  p4_2,
  p4_3,
  p4_4,
  p4_5,
  p4_6,
  p4_7,
  
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
