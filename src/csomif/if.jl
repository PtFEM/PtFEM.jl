
path = Pkg.dir("CSoM", "deps", "src", "CSoM", "4th_ed", "libcsom.")
path = path*@osx ? "dylib" : "so"

csom = dlopen(path)
formnf_ = dlsym(csom, :formnf_)
num_to_g_ = dlsym(csom, :num_to_g_)
fkdiag_ = dlsym(csom, :fkdiag_)
rigid_jointed_ = dlsym(csom, :rigid_jointed_)
fsparv_ = dlsym(csom, :fsparv_)
sparin_ = dlsym(csom, :sparin_)
spabac_ = dlsym(csom, :spabac_)

testf03_ = dlsym(csom, :testf03_)
