using CSoM

include(Pkg.dir("CSoM", "src", "CSoM", "exportVTK_XML.jl"))

old = pwd()
ProjDir = Pkg.dir("CSoM", "examples", "MaterialNonlinearity")
cd(ProjDir)

nodes = [
  VTKNode([0.0, 0.0, 0.0]),
  VTKNode([1.0, 0.0, 0.0]),
  VTKNode([1.0, 1.0, 0.0]),
  VTKNode([0.0, 1.0, 0.0]),
  VTKNode([0.5, 1.5, 0.0])
]

elements = [VTKElement([1, 2, 3, 4], 9), VTKElement([3, 4, 5], 5)]

write_VTKXML("example.vtu", nodes, elements, false)
write_VTKXML("example_bin.vtu", nodes, elements, true)
write_VTKXML("example_compressed_bin.vtu", nodes, elements, true, true)

cd(old)