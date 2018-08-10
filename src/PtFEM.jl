module PtFEM

using Compat, SuiteSparse, SparseArrays, LinearAlgebra, Printf, DataFrames, Statistics
using OffsetArrays, QuadGK
       
# package code goes here
### Imports ###

#using CustomUnitRanges: filename_for_zerorange
#include(filename_for_zerorange)

#=
_size(A::AbstractArray) = size(LinearIndices(A))
_size(A) = size(A)

_length(A::AbstractArray) = length(LinearIndices(A))
_length(A) = length(A)
=#

### Includes ###
include("PtFEM/Types/finite_elements.jl")
include("PtFEM/Types/structural_elements.jl")
include("PtFEM/Types/FEM.jl")

include("PtFEM/Main/formnf.jl")
include("PtFEM/Main/num_to_g.jl")
include("PtFEM/Main/fsparm.jl")
include("PtFEM/Main/rigid_jointed.jl")
include("PtFEM/Main/pin_jointed.jl")
include("PtFEM/Main/sample_hexahedron.jl")
include("PtFEM/Main/sample_line.jl")
include("PtFEM/Main/sample_quadrilateral.jl")
include("PtFEM/Main/sample_tetrahedron.jl")
include("PtFEM/Main/sample_triangle.jl")
include("PtFEM/Main/deemat.jl")
include("PtFEM/Main/bmat_nonaxi.jl")
include("PtFEM/Main/beemat.jl")
include("PtFEM/Main/shape_fun.jl")
include("PtFEM/Main/shape_der.jl")
include("PtFEM/Main/rod_km.jl")
include("PtFEM/Main/rod_mm.jl")
include("PtFEM/Main/beam_km.jl")
include("PtFEM/Main/beam_mm.jl")
include("PtFEM/Main/global_to_axial.jl")
include("PtFEM/Main/hinge.jl")
include("PtFEM/Main/global_to_local.jl")
include("PtFEM/Main/local_to_global.jl")
include("PtFEM/Main/checon.jl")
include("PtFEM/Main/stability.jl")
include("PtFEM/Main/beam_gm.jl")
include("PtFEM/Main/fmplat.jl")
include("PtFEM/Main/read_generated_data.jl")
include("PtFEM/Main/invar.jl")
include("PtFEM/Main/formm.jl")
include("PtFEM/Main/mocouf.jl")
include("PtFEM/Main/mocouq.jl")

include("PtFEM/Utilities/useplots.jl")
#if useplots()
#  include("graphics/plotrecipes/mesh.jl")
#end

include("PtFEM/Geom/bc_rect.jl")
include("PtFEM/Geom/mesh_size.jl")
include("PtFEM/Geom/geom_rect.jl")
include("PtFEM/Geom/hexahedron_xz.jl")

#include("graphics/vtkrecipes/vtk.jl")
#include("graphics/vtkrecipes/exportVTK_XML.jl")

include("PtFEM/Deprecated/fkdiag.jl")
include("PtFEM/Deprecated/fsparv.jl")
include("PtFEM/Deprecated/fromSkyline.jl")
include("PtFEM/Deprecated/skyline2sparse.jl")
include("PtFEM/Deprecated/linmul_sky.jl")
include("PtFEM/Deprecated/spabac.jl")
include("PtFEM/Deprecated/sparin.jl")

include("4 Static Equilibrium/p41.jl")
include("4 Static Equilibrium/p42.jl")
include("4 Static Equilibrium/p43.jl")
include("4 Static Equilibrium/p44.jl")
include("4 Static Equilibrium/p45.jl")
include("4 Static Equilibrium/p46.jl")
include("4 Static Equilibrium/p47.jl")

include("5 Elastic Solids/p51.jl")
include("5 Elastic Solids/p51_skyline.jl")
include("5 Elastic Solids/p52.jl")
include("5 Elastic Solids/p53.jl")
include("5 Elastic Solids/p54.jl")
include("5 Elastic Solids/p55.jl")
include("5 Elastic Solids/p56.jl")
include("5 Elastic Solids/p56_skyline.jl")

include("6 Material Nonlinearity/p61.jl")
include("6 Material Nonlinearity/p62.jl")
include("6 Material Nonlinearity/p62a.jl")
include("6 Material Nonlinearity/p63.jl")
include("6 Material Nonlinearity/p63_skyline.jl")

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
  jFEM,
  
  # From graphics/plotrecipes/mesh.jl
  mesh,
  
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
  p51_skyline,
  p52,
  p53,
  p54,
  p55,
  p56,
  p56_skyline,
  
  # From Chap05
  p61,
  p62,
  pp62,
  p62a,
  p63,
  p63_skyline,
  
  # From OffsetArrays
  OffsetArray

### Deprecated ###
  
  include("deprecated.jl")

###

end # module
