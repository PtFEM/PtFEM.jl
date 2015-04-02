using CSoM

old = pwd()
ProjDir = Pkg.dir("CSoM", "examples", "MaterialNonlinearity")
cd(ProjDir)

type Node
    coords::Vector{Float64}
end

type Element
    vertices::Vector{Int}
    vtknum::Int
end

include(Pkg.dir("CSoM", "src", "CSoM", "exportVTK_XML.jl"))

nodes = [Node([0.0, 0.0, 0.0]),
        Node([1.0, 0.0, 0.0]),
        Node([1.0, 1.0, 0.0]),
        Node([0.0, 1.0, 0.0]),
        Node([0.5, 1.5, 0.0])]

elements = [Element([1, 2, 3, 4], 9), Element([3, 4, 5], 5)]

write_VTKXML("example.vtu", nodes, elements, false)
write_VTKXML("example_bin.vtu", nodes, elements, true)
write_VTKXML("example_compressed_bin.vtu", nodes, elements, true, true)

cd(old)