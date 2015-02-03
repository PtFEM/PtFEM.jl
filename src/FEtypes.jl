abstract Element

type Beam <: Element
end

abstract FE_type

type Triangle <: FE_type
end

type Quadrilateral <: FE_type
end

type Hexahedron <: FE_type
end

