DepsDir = Pkg.dir("CSoM", "src", "deps")
path = Pkg.dir(DepsDir, "d3csom.so")
file = Pkg.dir(DepsDir, "normnf.f95")

cd(DepsDir)

run(`gfortran $(file) -o $(path) -shared -fPIC`)
