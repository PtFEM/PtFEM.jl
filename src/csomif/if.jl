
path = Pkg.dir("CSoM", "deps", "src", "CSoM", "4th_ed", "libd3csom4.")
@osx ? path*"dylib" : path*"so"

csom = dlopen(path)
formnf_ = dlsym(csom, :formnf_)
num_to_g_ = dlsym(csom, :num_to_g_)
fkdiag_ = dlsym(csom, :fkdiag_)
rigid_jointed_ = dlsym(csom, :rigid_jointed_)
fsparv_ = dlsym(csom, :fsparv_)
sparin_ = dlsym(csom, :sparin_)
spabac_ = dlsym(csom, :spabac_)

function num_to_g(g_num, nf, g)
  for i in 1:nels
    num = g_num[:, i]
    ccall(num_to_g_, Void,
      (Ptr{Int64}, Ptr{Int64}, Ptr{Int64}, Ptr{Int64}, Ptr{Int64}, Ptr{Int64}, Ptr{Int64}),
      &int64(nod), &int64(nodof), &int64(nn), &int64(ndof), num, nf, g
    )
    g_g[:, i] = g
    ccall(fkdiag_, Void,
      (Ptr{Int64}, Ptr{Int64}, Ptr{Int64}, Ptr{Int64}),
      &int64(ndof), &int64(neq), g, kdiag
    )
  end
  for i in 2:neq
    kdiag[i] = kdiag[i] + kdiag[i-1]
  end
  (nf, g_g, kdiag)
end
