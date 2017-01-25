using CSoM

if VERSION.minor == 5
  nodes = [
    VTKNode([0.0, 0.0, 0.0]),
    VTKNode([1.0, 0.0, 0.0]),
    VTKNode([1.0, 1.0, 0.0]),
    VTKNode([0.0, 1.0, 0.0]),
    VTKNode([0.5, 1.5, 0.0])
  ]

  elements = [VTKElement([1, 2, 3, 4], 9), VTKElement([3, 4, 5], 5)]
  
  file = joinpath(dirname(@__FILE__), "VTKexample.vtu")
  write_VTKXML(file, nodes, elements, false)
  #write_VTKXML("example_bin.vtu", nodes, elements, true)
  #write_VTKXML("example_compressed_bin.vtu", nodes, elements, true, true)
else
  print("\n\tTest temporarily disabled for 0.4!\n")
end