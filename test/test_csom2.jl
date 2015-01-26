using CSoM

DepsDir = Pkg.dir("CSoM", "deps", "src", "CSoM", "4th_ed")
path = Pkg.dir(DepsDir, "libd3csom4.")
path = path*@osx ? "dylib" : "so"

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

for i in 2:neq
  kdiag[i] = kdiag[i] + kdiag[i-1]
end

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

ccall(sparin_, Void,
  (Ptr{Int64}, Ptr{Int64}, Ptr{Float64}, Ptr{Int64}),
  &int64(kdiag[neq]), &int64(neq), kv, kdiag
)

ccall(spabac_, Void,
  (Ptr{Int64}, Ptr{Int64}, Ptr{Int64},
    Ptr{Float64}, Ptr{Float64}, Ptr{Int64}),
  &int64(kdiag[neq]), &int64(size(loads,1)), &int64(neq),
  kv, loads, kdiag
)

displacements = zeros(size(nf))
for i in 1:size(displacements, 1)
  for j in 1:size(displacements, 2)
    if nf[i, j] > 0
      displacements[i,j] = loads[nf[i, j]]
    end
  end
end

@assert round(displacements[2,1:7], 5) == [0.0  -0.00079  -0.00309  -0.00684  -0.01195  -0.01833  -0.02592]
@assert round(displacements[2,8:14], 5) == [-0.03463  -0.04437  -0.05508  -0.06667  -0.07905  -0.09216  -0.10591]
@assert round(displacements[2,15:21], 5) == [-0.12021  -0.135  -0.15019  -0.16569  -0.18144  -0.19735  -0.21333]

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

@assert round(actions[12, 16:20], 2) == [-8000.0  -6000.0  -4000.0  -2000.0  0.0]

cd(old)