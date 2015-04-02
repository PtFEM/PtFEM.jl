using LightXML
using Codecs

type VTKNode
  coords::Vector{Float64}
end

type VTKElement
  vertices::Vector{Int}
  vtknum::Int
end

abstract AbstractVTKXML

abstract AbstractVTKXMLBinary <: AbstractVTKXML

function add_data!{T <: AbstractVTKXMLBinary}(vtkxml::T, data)
    write(vtkxml.buffer, data)
end

type VTKXMLBinaryCompressed <: AbstractVTKXMLBinary
    buffer::IOBuffer
end
VTKXMLBinaryCompressed() = VTKXMLBinaryCompressed(IOBuffer())

function write_data!(vtkxml::VTKXMLBinaryCompressed, xmlele::XMLElement)
    uncompressed_size = vtkxml.buffer.size
    compressed_data = encode(Zlib, takebuf_array(vtkxml.buffer))
    compressed_size = length(compressed_data)
    header = UInt32[1, uncompressed_size, uncompressed_size, compressed_size]
    header_binary = bytestring(encode(Base64, reinterpret(UInt8, header)))
    data_binary = bytestring(encode(Base64, compressed_data))
    add_text(xmlele, header_binary)
    add_text(xmlele, data_binary)
end

type VTKXMLBinaryUncompressed <: AbstractVTKXMLBinary
    buffer::IOBuffer
end
VTKXMLBinaryUncompressed() = VTKXMLBinaryUncompressed(IOBuffer())

function write_data!(vtkxml::VTKXMLBinaryUncompressed, xmlele::XMLElement)
    uncompressed_size = vtkxml.buffer.size
    uncompressed_data = takebuf_array(vtkxml.buffer)
    header = UInt32[uncompressed_size]
    header_binary = bytestring(encode(Base64, reinterpret(UInt8, header)))
    data_binary = bytestring(encode(Base64, uncompressed_data))
    add_text(xmlele, header_binary)
    add_text(xmlele, data_binary)
end


type VTKXMLASCII <: AbstractVTKXML
    buffer::IOBuffer
end
VTKXMLASCII() = VTKXMLASCII(IOBuffer())


function add_data!(vtkxml::VTKXMLASCII, data)
    print(vtkxml.buffer, data, " ")
end

function add_data!{T <: AbstractArray}(vtkxml::VTKXMLASCII, data::T)
    for comp in data
        print(vtkxml.buffer, comp, " ")
    end
end

function write_data!(vtkxml::VTKXMLASCII, xmlele::XMLElement)
    add_text(xmlele, takebuf_string(vtkxml.buffer))
end

function write_VTKXML(filename::ASCIIString, nodes::Vector{VTKNode},
                             elements::Vector{VTKElement},
                             binary::Bool=true, compress::Bool=false)
     if (!binary) && compress
        error("Can only compress when using Binary format")
    end

    if binary
        if compress
            _write_VTKXML(filename, nodes, elements, binary, compress, VTKXMLBinaryCompressed())
        else
            _write_VTKXML(filename, nodes, elements, binary, compress, VTKXMLBinaryUncompressed())
        end
    else
        _write_VTKXML(filename, nodes, elements, binary, compress, VTKXMLASCII())
    end
end

function _write_VTKXML{P <: AbstractVTKXML}(filename::ASCIIString, nodes::Vector{VTKNode},
                                            elements::Vector{VTKElement}, binary::Bool,
                                            compress::Bool, vtkxml::P)

    if binary
        const VTK_FORMAT = "binary"
    else
        const VTK_FORMAT = "ascii"
    end

    xdoc = XMLDocument()
    xroot = create_root(xdoc, "VTKFile")
    set_attribute(xroot, "type", "UnstructuredGrid")
    set_attribute(xroot, "version", "0.1")
    set_attribute(xroot, "byte_order", "LittleEndian")

    if compress
        set_attribute(xroot, "compressor", "vtkZLibDataCompressor")
    end

    xgrid = new_child(xroot, "UnstructuredGrid")

    xpiece = new_child(xgrid, "Piece")
    set_attribute(xpiece, "NumberOfPoints", length(nodes))

    ncells = length(elements)
    set_attribute(xpiece, "NumberOfCells", length(elements))

    # Points
    xpoints = new_child(xpiece, "Points")

    # Coordinates for points
    xcoords = new_child(xpoints, "DataArray")
    set_attribute(xcoords, "type", "Float64")
    set_attribute(xcoords, "name", "Points")
    set_attribute(xcoords, "format", VTK_FORMAT)
    set_attribute(xcoords, "NumberOfComponents", "3")

    for node in nodes
        for coord in node.coords
            add_data!(vtkxml, coord)
        end
    end
    write_data!(vtkxml, xcoords)

    # Cells
    xcells = new_child(xpiece, "Cells")

    # Cell location
    xcellconn = new_child(xcells, "DataArray")
    set_attribute(xcellconn, "type", "Int64")
    set_attribute(xcellconn, "Name", "connectivity")
    set_attribute(xcellconn, "format", VTK_FORMAT)

    for element in elements
        for vertex in element.vertices
            add_data!(vtkxml, vertex-1)
        end
    end
    write_data!(vtkxml, xcellconn)


    xcell_offsets = new_child(xcells, "DataArray")
    set_attribute(xcell_offsets, "type", "Int64")
    set_attribute(xcell_offsets, "Name", "offsets")
    set_attribute(xcell_offsets, "format", VTK_FORMAT)
    offset = 0
    for element in elements
        offset += length(element.vertices)
        add_data!(vtkxml, offset)
    end
    write_data!(vtkxml, xcell_offsets)


    xcell_types = new_child(xcells, "DataArray")
    
    set_attribute(xcell_types, "type", "UInt8")
    set_attribute(xcell_types, "Name", "types")
    set_attribute(xcell_types, "format", VTK_FORMAT)
    for element in elements
      if VERSION >= v"0.4.0"
        add_data!(vtkxml, UInt8(element.vtknum))
      else
        add_data!(vtkxml, Uint8(element.vtknum))
      end
    end
    write_data!(vtkxml, xcell_types)

    save_file(xdoc, filename)
end


