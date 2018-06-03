"""

## stability

Function spabac! performs Cholesky forward and back-substitution on
a symmetric skyline global matrix. The loads vector is updated.

###Arguments
```julia
stability(gsm, ggm, tol, limit)
```

### Arguments
```julia
* gsm::SparseMatrixCSC{Float64,Int}   : Factored global stiffness matrix
* ggm::SparseMatrixCSC{Float64,Int}   : Factored geometric matrix
* tol::Float64                          : Convergence tolerance
* limit::Int                          : Iteration limit
```
"""
function stability(gsm::SparseMatrixCSC{Float64,Int}, 
  ggm::SparseMatrixCSC{Float64,Int}, tol::Float64, limit::Int)
  #
  # This subroutine computes the smallest eigenvalue in a beam
  # stability analysis. This is the Julia version. Notice no !.
  #
  # Updateds iters, evec, ival
  #
  converged = true
  neq=size(gsm, 1)
  x0 = zeros(neq+1)
  x1 = zeros(neq+1)
  big = 0.0
  
  cfgsm = cholesky(gsm)
  
  iters = 0
  x0[2] = 1.0
  
  while true
   iters = iters+1
   x1[2:end] = ggm * x0[2:end]
   x1[2:end] = cfgsm \ x1[2:end]
   big = maximum(x1[2:end])
   if abs(minimum(x1[2:end])) > big
     big = minimum(x1[2:end])
   end
   x1 = x1 / big
   converged = (maximum(abs.(x1[2:end]-x0[2:end]))/maximum(abs.(x1[2:end])) < tol)
   x0 = copy(x1)
   if converged || iters==limit
     break
   end
  end
  x1[2:end]=x1[2:end] / sqrt(sum(x1[2:end] .^ 2))
  ival = 1.0 / big
  (iters, x1, ival)
end