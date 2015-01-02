path = "/usr/local/Private/library/d3csom.so"
isfile(path) && rm(path)

run(`gfortran normnf.f95 -o $(path) -shared -fPIC`)
