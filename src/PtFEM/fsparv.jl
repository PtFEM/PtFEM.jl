"
Function fsparv! assembles fin_el matrices into a symmetric skyline
global matrix. The Skyline vector kv is updated.

Arguments to fsparv!(kv, km, g, kdiag):

kv::Vector{Float64}   : Skyline vector of global stiffness matrix.\n
km::Matrix{Float64}   : Stiffness matrix.\n
g::Vector{Int64}      : Global coordinate vector.\n
kdiag::Vector{Int64}  : Diagonal fin_el vector.
"
function fsparv!(kv::Vector{Float64}, km::Matrix{Float64},
  g::Vector{Int64}, kdiag::Vector{Int64})
  ndof = size(g, 1)
  for i in 1:ndof
    k = g[i]
    if k !== 0
      for j in 1:ndof
        if g[j] !== 0
          iw = k - g[j]
          if iw >= 0
            ival = kdiag[k] - iw
            kv[ival] += km[i, j]
          end
        end
      end
    end
  end
end

"
Function fsparm assembles fin_el matrices into a Julia sparse
global stiffness matrix.

Arguments to fsparm!(ssm, el, g, km):

ssm::SparseArrays{Float64, Float64}   : Sparse stiffnes matrix\n
el::Int64                             : Element
g::Vector{Int64}                      : Global coordinate vector.\n
km::Matrix{Float64}                   : Stiffness matrix.\n

Returns:

Updated ssm.

"
function fsparm!(gsm, g, km)
  #println("g = $g")
  ndof = size(g, 1)
  #println("ndof = $ndof")
  for i in 1:ndof
    k = g[i]
    #println("g[$i] = k = $k")
    if k !== 0
      for j in 1:ndof
        #println("i = $i, j = $j, g[$j] = $(g[j])")
        if g[j] !== 0
          iw = k - g[j]
          #println("iw = $iw")
          if iw >= 0
            if g[i] == g[j]
              gsm[g[i], g[j]] += km[i, j]
            else
              gsm[g[i], g[j]] += km[i, j]
              gsm[g[j], g[i]] += km[i, j]
            end  
            #println("gsm[$(g[i]), $(g[j])] += $(km[i,j])")
            #full(gsm) |> display
            #println()
          end
        end
      end
    end
  end
end
