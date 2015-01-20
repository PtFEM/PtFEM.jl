function num_to_g!(nod::Int64, nodof::Int64, nn::Int64, ndof::Int64,
  num::Vector{Int64}, nf::Matrix{Int64}, g::Vector{Int64})
  for i in 1:nod
    k = i*nodof
    g[k-nodof+1:k] = nf[:, num[i]]
  end
  #g
end