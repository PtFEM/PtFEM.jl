using PtFEM

if VERSION.minor == 5
  nodes = [
    VTKNode([0.0, 0.0, 0.0]),
    VTKNode([1.0, 0.0, 0.0]),
    VTKNode([1.0, 1.0, 0.0]),
    VTKNode([0.0, 1.0, 0.0]),
    VTKNode([0.5, 1.5, 0.0])
  ]

  fin_els = [VTKElement([1, 2, 3, 4], 9), VTKElement([3, 4, 5], 5)]
  
  file = joinpath(dirname(@__FILE__), "VTKexample.vtu")
  write_VTKXML(file, nodes, fin_els, false)
  #write_VTKXML("example_bin.vtu", nodes, fin_els, true)
  #write_VTKXML("example_compressed_bin.vtu", nodes, fin_els, true, true)
else
  print("\n\tTest temporarily disabled for 0.4!\n")
end