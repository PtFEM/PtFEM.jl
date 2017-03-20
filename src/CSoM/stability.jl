function stability!(kv::Vector{Float64}, gv::Vector{Float64}, kdiag::Vector{Int64}, tol::Float64, limit::Int64, iters::Int64, evec::Vector{Float64}, ival::Float64)
  #
  # This subroutine computes the smallest eigenvalue in a beam
  # stability analysis.
  #
  # Updateds iters, evec, ival
  #
  converged = true
  neq=size(kdiag,1)
  x0 = zeros(neq+1)
  x1 = zeros(neq+1)
  big = 0.0
  
  sparin!(kv,kdiag)
  
  iters = 0
  x0[2] = 1.0
  
  while true
   iters = iters+1
   x1[2:end] = linmul_sky!(gv, x0[2:end], x1[2:end], kdiag)
   x1[2:end] = spabac!(kv, x1[2:end], kdiag)  
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

function stability!(gsm::SparseMatrixCSC{Float64,Int64}, 
  ggm::SparseMatrixCSC{Float64,Int64}, tol::Float64, limit::Int64)
  #
  # This subroutine computes the smallest eigenvalue in a beam
  # stability analysis.
  #
  # Updateds iters, evec, ival
  #
  converged = true
  neq=size(gsm, 1)
  x0 = zeros(neq+1)
  x1 = zeros(neq+1)
  big = 0.0
  
  cfgsm = cholfact(gsm)
  
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