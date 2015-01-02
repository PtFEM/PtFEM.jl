using CSoM

DepsDir = Pkg.dir("CSoM", "src", "deps")
path = Pkg.dir(DepsDir, "d3csom.so")

old = pwd()
cd(DepsDir)

csom = dlopen(path)
formnf_ = dlsym(csom, :formnf_)
num_to_g_ = dlsym(csom, :num_to_g_)
fkdiag_ = dlsym(csom, :fkdiag_)
rigid_jointed_ = dlsym(csom, :rigid_jointed_)
fsparv_ = dlsym(csom, :fsparv_)
sparin_ = dlsym(csom, :sparin_)
spabac_ = dlsym(csom, :spabac_)

nels = 20                     # Number of elements
nn = 21                       # Number of nodes in the mesh
ndim = 3                      # Number of dimensions
nod = 2                       # Number of nodes per element
nprops = 4                    # Number of material properties
np_types = 1                  # Number of different property types

if ndim == 2
  nodof = 3                   # Number of degrees of freedom per node
elseif ndim == 3
  nodof = 6
end

ndof = nod * nodof            # Degrees of freedom per element
fixed_freedoms = 0            # Number of fixed displacement
loaded_nodes = 0              # Number of loaded nodes

# Int64 arrays
etype = int(ones(nels))
g = int(zeros(ndof))
g_g = int(zeros(ndof, nels))
g_num = int(zeros(nod, nels))
nf = int(ones(nodof, nn))
num = int(zeros(nod))

#neq = maximum(nf)
#kdiag = int(zeros(neq))

#no = int(zeros(xxx))
#node = int(zeros(xxx))
#sense = int(zeros(xxx))

# Float64 arrays
actions = zeros(ndof, nels)
coord = zeros(nod, ndim)
eld = zeros(ndof)
gamma = zeros(nels)
g_coord = zeros(ndim, nn)
km = zeros(ndof, ndof)

#kv = zeros(xxx)
#loads = zeros(xxx)

prop = reshape([2.0e6 1.0e6 1.0e6 3.0e5], nprops, np_types)
# if np_types > 1, get etype array
# if ndim == 3, get gamma array

# get g_coord array, in this case generate it, just x coordinate changes
g_coord[1,:] = linspace(0,4,21)

# Node numbering
g_num[1,:] = int(linspace(1, 20, 20))
g_num[2,:] = int(linspace(2, 21, 20))

# Get node constraints when appicable (leave at 1 when free)
nf[:, 1] = [0, 0, 0, 0, 0, 0]

ccall(formnf_, Void, 
  (Ptr{Int64}, Ptr{Int64}, Ptr{Int64}),
  &int64(nodof), &int64(nn), nf
)

println()
nf |> display
println()

neq = maximum(nf)
kdiag = int(zeros(neq))
loads = zeros(length(nf))

# Set loaded nodes
loads[nf[:, 21]] = [0.0 -10000.0 0.0 0.0 0.0 0.0]

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

g_g |> display
println()

for i in 2:neq
  kdiag[i] = kdiag[i] + kdiag[i-1]
end

println("kdiag (reshaped from $(typeof(kdiag)) of length $(size(kdiag, 1))):")
reshape(kdiag, 20,6) |> display
println()

kv = zeros(kdiag[neq])

println("There are $(neq) equations and the skyline storage is $(kdiag[neq]).\n")

# subroutine rigid_jointed(ndof, nprops, np_types, nels, nod, ndim, km, prop, gamma, etype, iel, coord) 

for i in 1:nels
  num = g_num[:, i]
  coord = g_coord[:, num]'              #'
  ccall(rigid_jointed_, Void,
    (Ptr{Int64}, Ptr{Int64}, Ptr{Int64}, Ptr{Int64}, Ptr{Int64}, Ptr{Int64},
      Ptr{Float64}, Ptr{Float64}, Ptr{Float64}, Ptr{Int64}, Ptr{Int64}, Ptr{Float64}),
    &int64(ndof), &int64(nprops), &int64(np_types), &int64(nels), &int64(nod), &int64(ndim),
    km, prop, gamma, etype, &int64(i), coord
  )
  g = g_g[:, i]
  ccall(fsparv_, Void,
    (Ptr{Int64}, Ptr{Int64}, Ptr{Int64},
      Ptr{Float64}, Ptr{Float64}, Ptr{Int64}, Ptr{Int64}),
    &int64(kdiag[neq]), &int64(ndof),  &int64(neq),
    kv, km, g, kdiag
  )
end

round(km) |> display
println()

println("First 10 elements of kv (of type $(typeof(kv)) with length $(size(kv, 1))):")
kv[1:10] |> display
println()

ccall(sparin_, Void,
  (Ptr{Int64}, Ptr{Int64}, Ptr{Float64}, Ptr{Int64}),
  &int64(kdiag[neq]), &int64(neq), kv, kdiag
)

println("First 10 elements of update kv (of type $(typeof(kv)) with length $(size(kv, 1))):")
kv[1:10] |> display
println()

ccall(spabac_, Void,
  (Ptr{Int64}, Ptr{Int64}, Ptr{Int64},
    Ptr{Float64}, Ptr{Float64}, Ptr{Int64}),
  &int64(kdiag[neq]), &int64(size(loads,1)), &int64(neq),
  kv, loads, kdiag
)

displacements = zeros(size(nf))
println("Displacements:")
for i in 1:size(displacements, 1)
  for j in 1:size(displacements, 2)
    if nf[i, j] > 0
      displacements[i,j] = loads[nf[i, j]]
    end
  end
end

displacements |> display
println()

actions = zeros(ndof, nels)
for i in 1:nels
  num = g_num[:, i]
  coord = g_coord[:, num]'              #'
  g = g_g[:, i]
  eld = zeros(length(g))
  for j in 1:length(g)
    if g[j] != 0
      eld[j] = loads[g[j]]
    end
  end
  ccall(rigid_jointed_, Void,
    (Ptr{Int64}, Ptr{Int64}, Ptr{Int64}, Ptr{Int64}, Ptr{Int64}, Ptr{Int64},
      Ptr{Float64}, Ptr{Float64}, Ptr{Float64}, Ptr{Int64}, Ptr{Int64}, Ptr{Float64}),
    &int64(ndof), &int64(nprops), &int64(np_types), &int64(nels), &int64(nod), &int64(ndim),
    km, prop, gamma, etype, &int64(i), coord
  )
  actions[:, i] = km * eld
end

println("Actions:")
actions |> display
println()

cd(old)