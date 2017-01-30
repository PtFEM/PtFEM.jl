function num_to_g!(nod::Int64, nodof::Int64, nn::Int64, ndof::Int64,
  num::Vector{Int64}, nf::Matrix{Int64}, g::Vector{Int64})
  #=
  println("nod = $nod")
  println("nodof = $nodof")
  println("nn = $nn")
  println("ndof = $ndof")
  println("num = $num")
  println("nf = $nf")
  println("g = $g")
  println(size(nf))
  println(size(g))
  =#
  for i in 1:nod
    k = i*nodof
    #println("[i,k,nodof,(k-nodof+1)] = $([i,k,nodof,(k-nodof+1)])")
    g[k-nodof+1:k] = nf[:, num[i]]
  end
  #g
end

function num_to_g!(num::Vector{Int64}, nf::Matrix{Int64}, g::Vector{Int64})
  nodof=size(nf,1)
  for i in 1:size(num, 1)
    k = i*nodof
    g[k-nodof+1:k] = nf[:, num[i]]
  end
  #g
end