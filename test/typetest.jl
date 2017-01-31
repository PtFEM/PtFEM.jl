using CSoM

data = Dict{Symbol, Any}(
  :struc_el => Beam1D(1, 1, 4, 1, :x, Line(2, 1), false)
)

function test_type(dct::Dict{Symbol, Any})
  exit_code::Int64 = 0
  
  if :struc_el in keys(data)
    struc_el::StructuralElement = data[:struc_el]
  else
    println("No fin_el type specified.")
    exit_code = 1
    return (exit_code, ndim, CSoM.UnknownStructuralElement())
  end
  
  ndim::Int64 = struc_el.ndim
  nst::Int64 = struc_el.nst
  
  # Add radial stress
  if ndim == 3 && struc_el.axisymmetric
    nst = 4
  end
  
  fin_el::FiniteElement = struc_el.fin_el
  @assert typeof(fin_el) <: FiniteElement
  
  if typeof(fin_el) == Line
    (nels, nn) = mesh_size(fin_el, struc_el.nxe)
  elseif typeof(fin_el) == Triangle || typeof(fin_el) == Quadrilateral
    (nels, nn) = mesh_size(fin_el, struc_el.nxe, struc_el.nye)
  elseif typeof(fin_el) == Hexahedron
    (nels, nn) = mesh_size(fin_el, struc_el.nxe, struc_el.nye, struc_el.nze)
  else
    println("$(typeof(fin_el)) is not a known finite element in an $(typeof(struc_el)).")
    exit_code = 2
    return (exit_code, ndim, CSoM.UnknownStructuralElement())
  end
  
  @show typeof(nels)
  @show typeof(nn)

  nodof::Int64 = fin_el.nodof         # Degrees of freedom per node
  ndof::Int64 = fin_el.nod * nodof    # Degrees of freedom per fin_el
  
  @show typeof(nodof)
  @show typeof(ndof)

  (exit_code, ndim, struc_el)
end

@code_warntype(test_type(data))
println()

#struc_el = data[:struc_el]
#fin_el = struc_el.fin_el
#@code_warntype(mesh_size(fin_el, struc_el.nxe))
#@neprintln()

println()
(exit_code, dim, eltype) = test_type(data)
println()
@show typeof(eltype)
@show typeof(dim)
println()

