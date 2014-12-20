using BucklingOfPipes

old = pwd()
ProjDir = Pkg.dir("BucklingOfPipes", "Examples", "CLT")
cd(ProjDir)

include(normpath(ProjDir, "clt1.jl"))

cd(old)