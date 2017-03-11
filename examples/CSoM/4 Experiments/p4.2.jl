using CSoM

ProjDir = dirname(@__FILE__)

l = 1.0       # Total length [m]
N = 5         # Number of nodes
els = N - 1   # Number of finite elements
nod = 2       # Number of nodes per finite elements
nodof = 1     # Degrees of freedom for each node
np_types = 1  # Number of proerty types
EA = 1.0e5    # Strain stiffness
nip = 1       # Number of integration points

struct_el = :Rod
fin_el = :Line

data = Dict(
  # Rod(nxe, np_types, nip, fin_el(nod, nodof))
  :struc_el => Rod(els, np_types, nip, Line(nod, nodof)),
  :properties => [EA;],
  :x_coords => 0.0:l/els:l,
);

data[:support] = [(N, [0])]
data[:loaded_nodes] = [(1, [5.0])]

# Parse & check FE problem data input dict

if :struc_el in keys(data)
  struc_el = data[:struc_el]
else
  throw("No structural element type specified.")
end

if typeof(struc_el) == CSoM.Rod
  ndim = 1
  nst = struc_el.np_types
else
  throw("FE4_1 expects a Rod structural element.")
end

fin_el = struc_el.fin_el
@assert typeof(fin_el) <: FiniteElement

if typeof(fin_el) == Line
  (nels, nn) = CSoM.mesh_size(fin_el, struc_el.nxe)
else
  throw("FE4_1 expects a Line (Interval) finite element.")
end
 
nodof = fin_el.nodof         # Degrees of freedom per node
ndof = fin_el.nod * nodof    # Degrees of freedom per fin_el

# Set penalty

penalty = :penalty in keys(data) ? data[:penalty] : 1e20

# All dynamic arrays

points = zeros(struc_el.nip, ndim)   # 
g = zeros(Int64, ndof)                   # Element steering vector
g_coord = zeros(ndim,nn)                 # 
fun = zeros(fin_el.nod)                 #
coord = zeros(fin_el.nod, ndim)         #
gamma = zeros(nels)                      #
jac = zeros(ndim, ndim)                  #
g_num = zeros(Int64, fin_el.nod, nels)  # 
der = zeros(ndim, fin_el.nod)           #
deriv = zeros(ndim, fin_el.nod)         #
bee = zeros(nst,ndof)                    #
km = zeros(ndof, ndof)                   #
mm = zeros(ndof, ndof)                   #
gm = zeros(ndof, ndof)                   #
kg = zeros(ndof, ndof)                   #
eld = zeros(ndof)                        #
weights = zeros(struc_el.nip)        #
g_g = zeros(Int64, ndof, nels)           #
num = zeros(Int64, fin_el.nod)          #
actions = zeros(nels, ndof)              #
nf = ones(Int64, nodof, nn)              #
displacements = zeros(size(nf, 1), ndim) #
gc = ones(ndim, ndim)                    #
dee = zeros(nst,nst)                     #
sigma = zeros(nst)                       #
axial = zeros(nels)                      #
x_coords = zeros(nn)                     #
y_coords = zeros(nn)                     # Not used, needed for FEM constructor
z_coords = zeros(nn)                     #
etype = ones(Int64, nels)                #
ell = zeros(nels)

# Start with arrays to be initialized from input dict

if :properties in keys(data)
  prop = zeros(size(data[:properties], 1), size(data[:properties], 2))
  for i in 1:size(data[:properties], 1)
    prop[i, :] = data[:properties][i, :]
  end
else
  throw("No :properties key found in input dict to FE4_1.")
end

# Update nodal freedom array from input dict :support entry
# nf has dimensions [nodof, nn] and all entries are set to 1 (free)

if :support in keys(data)
  for i in 1:size(data[:support], 1)
    nf[:, data[:support][i][1]] = data[:support][i][2]
  end
else
  throw("No :support key found in input dict to FE4_1.")
end

if :x_coords in keys(data)
  x_coords = data[:x_coords]
  @assert length(x_coords) == nels + 1
else
  dl = 1.0/nels
  x_coords = 0.0:dl:1.0
end

# Set lengths of elements
for i in 1:length(x_coords)-1
  ell[i] = x_coords[i+1] - x_coords[i]
end

# In multiple np_types, get np_type for each structural element
if :etype in keys(data)
  etype = data[:etype]
end

# Done with input dict and allocations, re-do nf (assign number to each dof)

CSoM.formnf!(nodof, nn, nf)
neq = maximum(nf)
kdiag = zeros(Int64, neq)

# Set global numbering, coordinates and array sizes

for i in 1:nels
  num = [i; i+1]
  CSoM.num_to_g!(fin_el.nod, nodof, nn, ndof, num, nf, g)
  g_g[:, i] = g
  CSoM.fkdiag!(ndof, neq, g, kdiag)
end

for i in 2:neq
  kdiag[i] = kdiag[i] + kdiag[i-1]
end

kv = zeros(kdiag[neq])
gv = zeros(kdiag[neq])

println("There are $(neq) equations and the skyline storage is $(kdiag[neq]).")
  
loads = zeros(neq+1)
if :loaded_nodes in keys(data)
  for i in 1:size(data[:loaded_nodes], 1)
    loads[nf[:, data[:loaded_nodes][i][1]]+1] = data[:loaded_nodes][i][2]
  end
end

ssm = spzeros(neq, neq)
for i in 1:nels
  km = CSoM.rod_km!(km, prop[etype[i], 1], ell[i])
  g = g_g[:, i]
  #println("element = $i: ")
  CSoM.fsparm!(ssm, i, g, km)
end

println()
ssm |> display
println()
full(ssm)
