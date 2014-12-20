using BucklingOfPipes

old = pwd()
ProjDir = Pkg.dir("BucklingOfPipes", "Examples", "CLT")
cd(ProjDir)

include(normpath(ProjDir, "clt2.jl"))

cd(old)
