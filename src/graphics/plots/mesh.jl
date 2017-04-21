function mesh(g_coord::Array{Float64,2}, g_num::Array{Int, 2})

  xa = Float64[]
  ya = Float64[]

  nod=size(g_num,1)
  nel=size(g_num,2)
  nod==5 && (nod=4)
  nod==9 && (nod=8)
  nod==10 && (nod=9)
  nod==15 && (nod=12)
  
  for i in 1:nel
    ii=g_num[1,i]
    ii==0 && continue
    append!(xa, g_coord[1,ii])
    append!(ya, g_coord[2,ii])
    for j in 2:nod
      jj=g_num[j,i]
      append!(xa, g_coord[1,jj])
      append!(ya, g_coord[2,jj])
    end
  end
  (xa, ya)
end