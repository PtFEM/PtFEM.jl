using CSoM

data = Dict{Symbol, Any}(
  :element_type => Beam(1, 1, 4, 1, :x, Line(2, 1), false)
)

function test_type(dct::Dict{Symbol, Any})
  exit_code::Int64 = 0
  
  if :element_type in keys(data)
    element_type::ElementType = data[:element_type]
  else
    println("No element type specified.")
    exit_code = 1
    return (exit_code, ndim, CSoM.UnknownElementType())
  end
  
  ndim::Int64 = element_type.ndim
  nst::Int64 = element_type.nst
  
  # Add radial stress
  if ndim == 3 && element_type.axisymmetric
    nst = 4
  end
  
  element::Element = element_type.element
  @assert typeof(element) <: Element
  
  if typeof(element) == Line
    (nels, nn) = mesh_size(element, element_type.nxe)
  elseif typeof(element) == Triangle || typeof(element) == Quadrilateral
    (nels, nn) = mesh_size(element, element_type.nxe, element_type.nye)
  elseif typeof(element) == Hexahedron
    (nels, nn) = mesh_size(element, element_type.nxe, element_type.nye, element_type.nze)
  else
    println("$(typeof(element)) is not a known finite element in an $(typeof(element_type)).")
    exit_code = 2
    return (exit_code, ndim, CSoM.UnknownElementType())
  end
  
  @show typeof(nels)
  @show typeof(nn)

  nodof::Int64 = element.nodof         # Degrees of freedom per node
  ndof::Int64 = element.nod * nodof    # Degrees of freedom per element
  
  @show typeof(nodof)
  @show typeof(ndof)

  (exit_code, ndim, element_type)
end

@code_warntype(test_type(data))
println()

#element_type = data[:element_type]
#element = element_type.element
#@code_warntype(mesh_size(element, element_type.nxe))
#@neprintln()

println()
(exit_code, dim, eltype) = test_type(data)
println()
@show typeof(eltype)
@show typeof(dim)
println()

