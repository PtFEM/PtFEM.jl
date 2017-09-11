
using JuAFEM
using UnicodePlots

ProjDir = dirname(@__FILE__)
cd(ProjDir) do

  grid = generate_grid(Quadrilateral, (20,20))
  addnodeset!(grid, "boundary", x -> abs(x[1]) ≈ 1 ||  abs(x[2]) ≈ 1);

  dim = 2
  ip = Lagrange{dim, RefCube, 1}()
  qr = QuadratureRule{dim, RefCube}(2)
  cellvalues = CellScalarValues(qr, ip);

  dh = DofHandler(grid)
  push!(dh, :T, 1) # Add a temperature field
  close!(dh)

  dbc = DirichletBoundaryConditions(dh)
  add!(dbc, :T, getnodeset(grid, "boundary"), (x,t) -> 0.0)
  close!(dbc)
  update!(dbc, 0.0)

  K = create_sparsity_pattern(dh);
  fill!(K.nzval, 1.0);
  spy(K)

  function doassemble(cellvalues::CellScalarValues{dim}, K::SparseMatrixCSC, dh::DofHandler) where dim
      b = 1.0
      f = zeros(ndofs(dh))
      assembler = start_assemble(K, f)
    
      n_basefuncs = getnbasefunctions(cellvalues)
      global_dofs = zeros(Int, ndofs_per_cell(dh))

      fe = zeros(n_basefuncs) # Local force vector
      Ke = zeros(n_basefuncs, n_basefuncs) # Local stiffness mastrix

      @inbounds for (cellcount, cell) in enumerate(CellIterator(dh))
          fill!(Ke, 0)
          fill!(fe, 0)
        
          reinit!(cellvalues, cell)
          for q_point in 1:getnquadpoints(cellvalues)
              dΩ = getdetJdV(cellvalues, q_point)
              for i in 1:n_basefuncs
                  δT = shape_value(cellvalues, q_point, i)
                  ∇δT = shape_gradient(cellvalues, q_point, i)
                  fe[i] += (δT * b) * dΩ
                  for j in 1:n_basefuncs
                      ∇T = shape_gradient(cellvalues, q_point, j)
                      Ke[i, j] += (∇δT ⋅ ∇T) * dΩ
                  end
              end
          end
        
          celldofs!(global_dofs, cell)
          assemble!(assembler, global_dofs, fe, Ke)
      end
      return K, f
  end;

  K, f = doassemble(cellvalues, K, dh);

  apply!(K, f, dbc)
  T = K \ f;

  vtkfile = vtk_grid("heat", dh, T)
  vtk_save(vtkfile);

  Base.Test.@test maximum(T) ≈ 0.29526786377073544
  println("Heat square successful")

end