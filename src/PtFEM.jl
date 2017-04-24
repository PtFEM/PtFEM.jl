module PtFEM

using DataTables, Compat

using OffsetArrays
       
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
include("graphics/plotrecipes/mesh.jl")
include("graphics/vtkrecipes/vtk.jl")

include("4 Static Equilibrium/p41.jl")
include("4 Static Equilibrium/p42.jl")
include("4 Static Equilibrium/p43.jl")
include("4 Static Equilibrium/p44.jl")
include("4 Static Equilibrium/p45.jl")
include("4 Static Equilibrium/p46.jl")
include("4 Static Equilibrium/p47.jl")

include("5 Elastic Solids/p51.jl")
include("5 Elastic Solids/p52.jl")
include("5 Elastic Solids/p53.jl")
include("5 Elastic Solids/p54.jl")
include("5 Elastic Solids/p55.jl")
include("5 Elastic Solids/p56.jl")

include("6 Material Nonlinearity/p61.jl")
include("6 Material Nonlinearity/p62.jl")
include("6 Material Nonlinearity/p62a.jl")

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
  
  # From graphics/plotrecipes/mesh.jl
  mesh,
  
  write_VTKXML,
  VTKNode,
  VTKElement,
  
  fromSkyline,
  skyline2sparse,
  
  # From Chap04
  p41,
  p42,
  p43,
  p44,
  p45,
  p46,
  p47,
  
  # From Chap05
  p51,
  p52,
  p53,
  p54,
  p55,
  p56,
  
  # From Chap05
  p61,
  p62,
  pp62,
  p62a,
  
  # From OffsetArrays
  OffsetArray
  
### Deprecated ###
  
  include("deprecated.jl")

end # module
