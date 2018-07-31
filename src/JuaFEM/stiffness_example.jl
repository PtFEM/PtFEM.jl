using JuAFEM
using Tensors

# Stiffness using normal matrices
function ke_element_mat!(Ke, X::Vector{Vec{dim, T}}, fe_values::CellScalarValues{dim}, Ee, B, DB, BDB) where {T, dim}
    n_basefuncs = getnbasefunctions(fe_values)
    @assert length(X) == n_basefuncs
    
    reinit!(fe_values, X)
    for q_point in 1:getnquadpoints(fe_values)
        for i in 1:n_basefuncs
            dNdx = shape_gradient(fe_values, q_point, i)[1]
            dNdy = shape_gradient(fe_values, q_point, i)[2]
            dNdz = shape_gradient(fe_values, q_point, i)[3]

            B[1, i * 3-2] = dNdx
            B[2, i * 3-1] = dNdy
            B[3, i * 3-0] = dNdz
            B[4, 3 * i-1] = dNdz
            B[4, 3 * i-0] = dNdy
            B[5, 3 * i-2] = dNdz
            B[5, 3 * i-0] = dNdx
            B[6, 3 * i-2] = dNdy
            B[6, 3 * i-1] = dNdx
        end
        
        A_mul_B!(DB, Ee, B)
        At_mul_B!(BDB, B, DB)
        scale!(BDB, getdetJdV(fe_values, q_point))
        for p in 1:size(Ke,1)
            for q in 1:size(Ke,2)
                Ke[p, q] += BDB[p, q]
            end
        end
    end
    
    return Ke
end;

# Stiffness using scalar values
function ke_element!(Ke, X::Vector{Vec{dim, T}}, fe_values::CellScalarValues{dim}, C) where {T,dim}
    n_basefuncs = getnbasefunctions(fe_values)
    @assert length(X) == n_basefuncs
    reinit!(fe_values, X)
    @inbounds for q_point in 1:getnquadpoints(fe_values)
        for a in 1:n_basefuncs
            for b in 1:n_basefuncs
                ∇ϕa = shape_gradient(fe_values, q_point, a)
                ∇ϕb = shape_gradient(fe_values, q_point, b)
                Ke_e = dotdot(∇ϕa, C, ∇ϕb) * getdetJdV(fe_values, q_point)
                for d1 in 1:dim, d2 in 1:dim
                    Ke[dim*(a-1) + d1, dim*(b-1) + d2] += Ke_e[d1,d2]
                end
            end
        end
    end
    return Ke
end;

# Stiffness using vector values
function ke_element2!(Ke, X::Vector{Vec{dim, T}}, fe_values::CellVectorValues{dim}, C) where {T,dim}
    n_basefuncs = getnbasefunctions(fe_values)
    @assert length(X) * dim == n_basefuncs
    reinit!(fe_values, X)
    ɛ = [zero(SymmetricTensor{2, dim, T}) for i in 1:n_basefuncs]
    @inbounds for q_point in 1:getnquadpoints(fe_values)
        for i in 1:n_basefuncs
            ɛ[i] = symmetric(shape_gradient(fe_values, q_point, i)) 
        end
        dΩ = getdetJdV(fe_values, q_point)
        for i in 1:n_basefuncs
            ɛC = ɛ[i] ⊡ C
            for j in 1:n_basefuncs
                Ke[i, j] += (ɛC ⊡ ɛ[j]) * dΩ
            end
        end
    end
    return Ke
end;

E = 200e9
ν = 0.3
λ = E*ν / ((1 + ν) * (1 - 2ν))
μ = E / (2(1 + ν))
δ(i,j) = i == j ? 1.0 : 0.0
g(i,j,k,l) = λ*δ(i,j)*δ(k,l) + μ*(δ(i,k)*δ(j,l) + δ(i,l)*δ(j,k))

C = SymmetricTensor{4, 3}(g)


M = λ/ν * (1 - ν)

Cmat = [ M      λ      λ    0.0    0.0   0.0;
         λ      M      λ    0.0    0.0   0.0;
         λ      λ      M    0.0    0.0   0.0;
        0.0    0.0    0.0    μ     0.0   0.0;
        0.0    0.0    0.0   0.0     μ    0.0;
        0.0    0.0    0.0   0.0    0.0    μ]


interpolation = Lagrange{3, RefCube, 1}()
quad_rule = QuadratureRule{3, RefCube}(1)
values = CellScalarValues(quad_rule, interpolation);
vector_values = CellVectorValues(quad_rule, interpolation);

# Generate some coordinates
x = [-1.0 -1.0 -1.0;
      1.0 -1.0 -1.0;
      1.0  1.0 -1.0;
     -1.0  1.0 -1.0;
     -1.0 -1.0  1.0;
      1.0 -1.0  1.0;
      1.0  1.0  1.0;
     -1.0  1.0  1.0;]
x = x .+ 0.05 * rand()
x_vec = reinterpret(Vec{3, Float64}, x, (8,));

n_basefunctions = getnbasefunctions(vector_values)
Ke = zeros(n_basefunctions, n_basefunctions)
Ke2 = copy(Ke)
Ke3 = copy(Ke)

B   =  zeros(6, n_basefunctions)
DB  =  zeros(6, n_basefunctions)
BDB =  zeros(n_basefunctions, n_basefunctions);

fill!(Ke, 0)
fill!(Ke2, 0)
fill!(Ke3, 0)
ke_element!(Ke2, x_vec, values, C)
ke_element2!(Ke3, x_vec, vector_values, C);
ke_element_mat!(Ke, x_vec, values, Cmat, B, DB, BDB);

using Test
@test norm(Ke - Ke2) / norm(Ke) < 1e-14
@test norm(Ke - Ke3) / norm(Ke) < 1e-14

println("Stiffness successful")

Ke


