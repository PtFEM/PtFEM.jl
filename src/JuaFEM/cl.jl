
using JuAFEM
using Tensors
using TimerOutputs
using UnicodePlots

const to = TimerOutput();
const dim = 3

ProjDir = dirname(@__FILE__)
cd(ProjDir) #do
  
  hex = true
  geoshape = hex ? Hexahedron : Tetrahedron
  refshape = hex ? RefCube    : RefTetrahedron
  order = hex ? 2 : 1;

  corner1 = Vec{dim}((0.0, 0.0, 0.0))
  corner2 = Vec{dim}((2.0, 2.0, 2.0))
  grid = generate_grid(geoshape, (2, 2, 2), corner1, corner2);
  # Extract the left boundary
  addnodeset!(grid, "clamped", x -> norm(x[1]) ≈ 0.0);

  # Interpolations and values
  interpolation_space = Lagrange{dim, refshape, 1}()
  quadrature_rule = QuadratureRule{dim, refshape}(order)
  cellvalues = CellVectorValues(quadrature_rule, interpolation_space);
  facevalues = FaceVectorValues(QuadratureRule{dim-1, refshape}(order), interpolation_space);

  # DofHandler
  dh = DofHandler(grid)
  push!(dh, :u, dim) # Add a displacement field
  close!(dh)

  @time K = create_symmetric_sparsity_pattern(dh); # assemble only upper half since it is symmetric
  fill!(K.data.nzval, 1.0);
  spy(K.data) |> display

  # Boundaryconditions
  dbc = DirichletBoundaryConditions(dh)
  # Add a homogenoush boundary condition on the "clamped" edge
  add!(dbc, :u, getnodeset(grid, "clamped"), (x,t) -> [0.0, 0.0, 0.0], collect(1:dim))
  close!(dbc)
  t = 0.0
  update!(dbc, t)

  # Create the stiffness tensor
  E = 200e9
  ν = 0.3
  λ = E*ν / ((1+ν) * (1 - 2ν))
  μ = E / (2(1+ν))
  δ(i,j) = i == j ? 1.0 : 0.0
  g(i,j,k,l) = λ*δ(i,j)*δ(k,l) + μ*(δ(i,k)*δ(j,l) + δ(i,l)*δ(j,k))
  C = SymmetricTensor{4, dim}(g);

  function doassemble(cellvalues::CellVectorValues{dim}, facevalues::FaceVectorValues{dim}, 
                      K::Symmetric, grid::Grid, dh::DofHandler, C::SymmetricTensor{4, dim}) where dim

    
      f = zeros(ndofs(dh))
      assembler = start_assemble(K, f)
    
      n_basefuncs = getnbasefunctions(cellvalues)

      fe = zeros(n_basefuncs) # Local force vector
      Ke = Symmetric(zeros(n_basefuncs, n_basefuncs), :U) # Local stiffness mastrix
    
      t = Vec{3}((0.0, 1e7, 0.0)) # Traction vector
      b = Vec{3}((0.0, 0.0, 0.0)) # Body force
      ɛ = [zero(SymmetricTensor{2, dim}) for i in 1:n_basefuncs]
      @inbounds for (cellcount, cell) in enumerate(CellIterator(dh))
          @timeit to "assem" begin
          fill!(Ke.data, 0)
          fill!(fe, 0)
        
          reinit!(cellvalues, cell)
          for q_point in 1:getnquadpoints(cellvalues)
              for i in 1:n_basefuncs
                  ɛ[i] = symmetric(shape_gradient(cellvalues, q_point, i)) 
              end
              dΩ = getdetJdV(cellvalues, q_point)
              for i in 1:n_basefuncs
                  δu = shape_value(cellvalues, q_point, i)
                  fe[i] += (δu ⋅ b) * dΩ
                  ɛC = ɛ[i] ⊡ C
                  for j in i:n_basefuncs # assemble only upper half
                      Ke.data[i, j] += (ɛC ⊡ ɛ[j]) * dΩ # can only assign to parent of the Symmetric wrapper
                  end
              end
          end
        
          for face in 1:nfaces(cell)
              if onboundary(cell, face) && (cellcount, face) ∈ getfaceset(grid, "right")
                  reinit!(facevalues, cell, face)
                  for q_point in 1:getnquadpoints(facevalues)
                      dΓ = getdetJdV(facevalues, q_point)
                      for i in 1:n_basefuncs
                          δu = shape_value(facevalues, q_point, i)
                          fe[i] += (δu ⋅ t) * dΓ
                      end
                  end
              end
          end
          global_dofs = celldofs(cell)
          assemble!(assembler, global_dofs, fe, Ke)
          end # timer
      end
      return K, f
  end;

  global K, f, u
  reset_timer!(to)
  K, f = doassemble(cellvalues, facevalues, K, grid, dh, C);
  print_timer(to; linechars = :ascii)

  # Modify K and f such that K \ f gives correct boundary conditions
  @time apply!(K, f, dbc)

  @time u = cholesky(K) \ f;

  # Save file
  vtkfile = vtk_grid("cl", dh, u)
  vtk_save(vtkfile)

  #Base.Test.@test maximum(u)  ≈ 1.919600482922295
  
  grid |> display
  println()
  
  grid.cells |> display
  println()
  
  grid.nodes |> display
  println()
  
  grid.facesets |> display
  println()
  
  println("Cantilever successful: $(reshape(u, Int(length(u)/3), 3)[end,:])")

  #end
