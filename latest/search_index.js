var documenterSearchIndex = {"docs": [

{
    "location": "INTRO.html#",
    "page": "Introduction",
    "title": "Introduction",
    "category": "page",
    "text": ""
},

{
    "location": "INTRO.html#Introduction-1",
    "page": "Introduction",
    "title": "Introduction",
    "category": "section",
    "text": ""
},

{
    "location": "INTRO.html#PtFEM-toolkit-1",
    "page": "Introduction",
    "title": "PtFEM toolkit",
    "category": "section",
    "text": "This Julia package will contain the programs in \"Programming the Finite Element Method\" by I M Smith, D V Griffiths and L. Margetts (PtFEM). See TODO for the planned progress.PtFEM is a very versatile toolkit to construct FEM programs for practical engineering and scientific problems (to distinguish it somewhat from systems primarily focused on solving symbolic partial differential equations). Each chapter in the book gradually develops a set of related programs intended to be used as a starting point for a particular class of problems.I use PtFEM when referring to the book and PtFEM.jl when referring to the Julia package. PtFEM.jl is the central package in a mini-PtFEM-ecosystem and most other packages will be using PtFEM.jl as the starting point."
},

{
    "location": "INTRO.html#A-Julia-based-PtFEM-eco-system-1",
    "page": "Introduction",
    "title": "A Julia based PtFEM eco system",
    "category": "section",
    "text": "PtFEM.jl is the central package in a mini PtFEM eco system and most other packages in that eco system will be using PtFEM.jl as the starting point.For many years a colleague and I have used the Fortran version of the PtFEM toolkit  to verify aspects of a (larger) software program developed to analyse the behavior of bottom hole assemblies (BHAs). PtFEM has proven extremely valuable for that purpose and in my opinion that alone justifies the creation of PtFEM.jl.We are now at a point where we would like to publish the results of our work in a reproducable (and maintainable) format. Thus an important secundairy motivation for creating PtFEM.jl is to be able to subsequently publish 2 additional packages, BHATheoreticalPerformance.jl and BHALockup.jl, on the basis of a well documented toolkit.The goal of the PtFEM eco system is to not only make our results easily reproducable but also to potentially capture broader usage of PtFEM, e.g. see the examples towards the end of  TODO. If there is long term interest to contribute to the PtFEM organisation, please consider becoming a team member or outside collaborator.The PtFEM eco system exists as a Github organization. A GitHub organisation is basically a place to collect a set of related packages, in this case Julia packages around the \"Programming the Finite Element Method\" toolkit.Occasionally a package might be temporarily selfstanding, e.g. ClassicalLaminateTheory.jl at present. The intention is to in the future upgrade/extend/incorporate that package, e.g. in the case of ClassicalLaminateTheory.jl to handle composite Bottom Hole Assembly modeling or extended reach casing installation.A companion package to PtFEM, NMfE.jl along the lines of Numerical Methods for Engineers by I M Smith and D V Griffiths, is also included in the PtFEM organisation but development work for this educational set of programs is happening on a slower pace because:In my mind it should form a bridge to some of the Julia packages mentioned in the README under related packages and focused on solving differential equations\nIt is less directly derived from the original Fortran programs, e.g. the symbolic examples in chapter 7 of NMfE using the Julia Symata package."
},

{
    "location": "GETTINGSTARTED.html#",
    "page": "Getting started",
    "title": "Getting started",
    "category": "page",
    "text": ""
},

{
    "location": "GETTINGSTARTED.html#Getting-started-1",
    "page": "Getting started",
    "title": "Getting started",
    "category": "section",
    "text": ""
},

{
    "location": "GETTINGSTARTED.html#Installation-1",
    "page": "Getting started",
    "title": "Installation",
    "category": "section",
    "text": "To use the toolkit and run the test programs, start the Julia REPL and type:Pkg.clone(\"https://github.com/PtFEM/PtFEM.jl\")\nPkg.test(\"PtFEM\")"
},

{
    "location": "GETTINGSTARTED.html#An-example-(Ex41.1.jl)-1",
    "page": "Getting started",
    "title": "An example (Ex41.1.jl)",
    "category": "section",
    "text": "using PtFEM\n\nProjDir = dirname(@__FILE__)\n\nl = 1.0       # Total length [m]\nN = 5         # Number of nodes\nels = N - 1   # Number of finite elements (in x direction)\nnod = 2       # Number of nodes per finite elements\nnodof = 1     # Degrees of freedom for each node\nnp_types = 1  # Number of property types\nEA = 1.0e5    # Strain stiffness\nnip = 1       # Number of integration points\n\ndata = Dict(\n  # StructuralElement(nxe, np_types, nip, FiniteElement(nod, nodof))\n  :struc_el => Rod(els, np_types, nip, Line(nod, nodof)),\n  :properties => [EA;],\n  # Compute x_coords using length l and number of elements, els\n  :x_coords => 0.0:l/els:l,\n  # Define a support for node N\n  # In this case fix the single dof (x direction displacement)\n  :support => [(N, [0])],\n  # External forces are applied to nodes 1 to 5.\n  :loaded_nodes => [\n      (1, [-0.625]),\n      (2, [-1.25]),\n      (3, [-1.25]),\n      (4, [-1.25]),\n      (5, [-0.625])\n    ]\n);\n\n# Display the data dictionary\ndata |> display\nprintln()\n\n# Solve the FEM model\n@time fem, dis_dt, fm_dt = p41(data)\nprintln()\n\ndisplay(dis_dt)\nprintln()\ndisplay(fm_dt)\nprintln()\n\n# Use Plots to generate a graphical representation of the result\n\nif VERSION.minor < 6\n\n  using Plots\n  gr(size=(400,500))\n\n  x = 0.0:l/els:l\n  u = convert(Array, dis_dt[:x_translation])\n    \n  p = Vector{Plots.Plot{Plots.GRBackend}}(2)\n  titles = [\"PtFEM Ex41.1 u(x)\", \"PtFEM Ex41.1 N(x)\"]\n   \n  p[1]=plot(ylim=(0.0, 1.0), xlim=(0.0, 5.0),\n    yflip=true, xflip=false, xlab=\"Normal force [N]\",\n    ylab=\"x [m]\", title=titles[2]\n  )\n  vals = convert(Array, fm_dt[:normal_force_2])\n  for i in 1:els\n      plot!(p[1], \n        [vals[i], vals[i]],\n        [(i-1)*l/els, i*l/els], color=:blue,\n        color=:blue, fill=true, fillalpha=0.1, leg=false\n      )\n      delta = abs(((i-1)*l/els) - (i*l/els)) / 20.0\n      y1 = collect(((i-1)*l/els):delta:(i*l/els))\n      for j in 1:length(y1)\n        plot!(p[1],\n          [vals[i], 0.0],\n          [y1[j], y1[j]], color=:blue, alpha=0.5\n        )\n      end\n  end\n  \n  p[2] = plot(u, x, xlim=(-0.00003, 0.0), yflip=true,\n    xlab=\"Displacement [m]\", ylab=\"x [m]\",\n    fillto=0.0, fillalpha=0.1, leg=false, title=titles[1])\n\n  plot(p..., layout=(1, 2))\n  savefig(ProjDir*\"/Ex41.1.png\")\n  \nend"
},

{
    "location": "CHANGES.html#",
    "page": "Changes w.r.t. PtFEM",
    "title": "Changes w.r.t. PtFEM",
    "category": "page",
    "text": ""
},

{
    "location": "CHANGES.html#Changes-with-respect-to-the-PtFEM-book-1",
    "page": "Changes w.r.t. PtFEM",
    "title": "Changes with respect to the PtFEM book",
    "category": "section",
    "text": "The PtFEM book is the primary source to understand how the Fortran toolkit can be used to build FEM programs. This is also the case for the Julia version of the PtFEM toolkit, PtFEM.jl.But even with this restriction in place, there are many ways to port the PtFEM toolkit to Julia. Julia can in fact call the lower level Fortran \"building blocks\" (subroutines) directly. But that would make it harder to modify those functions. PtFEM.jl is entirely written in Julia end takes a middle of the road approach in replacing Fortran functionality by \"typical\" Julia features. These cases are documented in this file.If additional Julia versions of functions, particularly \"building blocks\", are required for use in the programs, these are added to the respective source files. Often times Julia's \"multiple dispatch\" takes care of selecting the correct version in the templates."
},

{
    "location": "CHANGES.html#Julia's-convention-for-functions-that-update-arguments-1",
    "page": "Changes w.r.t. PtFEM",
    "title": "Julia's convention for functions that update arguments",
    "category": "section",
    "text": "Note the use of the \"!\" in some function names which is the Julia convention for functions that update one or more of the function arguments. "
},

{
    "location": "CHANGES.html#Custom-array-indices-1",
    "page": "Changes w.r.t. PtFEM",
    "title": "Custom array indices",
    "category": "section",
    "text": "Julia by default uses 1 as the first index into an array, but has the ability to use arbitrary indexing as well. The PtFEM Fortran programs use 0-based indexing for the loads vector. In programs p41 through to p44 in chapter 4 I have used OffsetArrays.jl for this purpose, i.e:using OffsetArrays\nN = 10\nloads = OffsetArray(zeros(N+1), 0:N)I'm planning to use the same approach in all other chapters."
},

{
    "location": "CHANGES.html#Replacing-skyline-storage-by-Julia-sparse-matrices-1",
    "page": "Changes w.r.t. PtFEM",
    "title": "Replacing skyline storage by Julia sparse matrices",
    "category": "section",
    "text": "In the programs for chapter 4, the skyline storage idea has been replaced by Julia sparse matrices and, accordingly, PtFEM's pair sparin() and spabac() by Julia's cholfact() and \"\\\" operator.Thus  PtFEM.sparin!(kv, kdiag)\n  loads[2:end] = PtFEM.spabac!(kv, loads[2:end], kdiag)has been replaced by  # Cholesky decomposed global stiffness matrix\n  cfgsm = cholfact(gsm)\n  loads[2:end] = cfgsm \\ loads[2:end]All 'basic' functions such as sparin!() and spabac!() can be found in the src/PtFEM directory."
},

{
    "location": "CHANGES.html#Separate-equivalent-loads-in-data-dictionary-1",
    "page": "Changes w.r.t. PtFEM",
    "title": "Separate equivalent loads  in data dictionary",
    "category": "section",
    "text": "E.g. p44. In p44 corrections are applied if :eq_nodal_forces_and_moments is defined in the data dictionary.See PtFEM/EEM.jl for further examples."
},

{
    "location": "CHANGES.html#Graphics-1",
    "page": "Changes w.r.t. PtFEM",
    "title": "Graphics",
    "category": "section",
    "text": "Graphics will be mostly implemented using the Julia pacckage Plots.jl (using the GR.jl backend)."
},

{
    "location": "CHANGES.html#Plots.jl-1",
    "page": "Changes w.r.t. PtFEM",
    "title": "Plots.jl",
    "category": "section",
    "text": "E.g. Ex41.1.jl, Ex61.1.jl and Ex62.1.jlSeveral programs will generate VTK output."
},

{
    "location": "CHANGES.html#VTK-(ParaView)-1",
    "page": "Changes w.r.t. PtFEM",
    "title": "VTK (ParaView)",
    "category": "section",
    "text": "E.g. Ex47.1.jl"
},

{
    "location": "CHANGES.html#Initial-introduction-of-parallel-programming-in-Julia-1",
    "page": "Changes w.r.t. PtFEM",
    "title": "Initial introduction of parallel programming in Julia",
    "category": "section",
    "text": "Some examples will show simple ways of using Julia's capabilities in this area.In Chapter 6, example Exp62.1a.jl calls p62a.jl which uses Julia pmap() for this purpose. This is too small a problem to really show performance improvements, but it shows an easy approach.Detail: Example Ex62.1a.jl could have called the p62(data) as Ex62.1.jl does; pp62(data) is identical but produces less output.Note:  have not done a profiling pass through p62.jl, it allocates way too much memory, so I expect significant performance improvements are possible."
},

{
    "location": "CHANGES.html#Integration-for-now-using-PtFEM's-approach-1",
    "page": "Changes w.r.t. PtFEM",
    "title": "Integration - for now using PtFEM's approach",
    "category": "section",
    "text": "Currently I have not replaced numerical integration by e.g. Julia quadgk() for 1D integration of a function."
},

{
    "location": "CHANGES.html#Gradient-descent-for-now-using-PtFEM's-approach-1",
    "page": "Changes w.r.t. PtFEM",
    "title": "Gradient descent - for now using PtFEM's approach",
    "category": "section",
    "text": ""
},

{
    "location": "CHANGES.html#Derivatives-for-now-using-PtFEM's-approach-1",
    "page": "Changes w.r.t. PtFEM",
    "title": "Derivatives - for now using PtFEM's approach",
    "category": "section",
    "text": ""
},

{
    "location": "index.html#",
    "page": "PtFEM.jl documentation",
    "title": "PtFEM.jl documentation",
    "category": "page",
    "text": ""
},

{
    "location": "index.html#Programs-1",
    "page": "PtFEM.jl documentation",
    "title": "Programs",
    "category": "section",
    "text": ""
},

{
    "location": "index.html#PtFEM.p41",
    "page": "PtFEM.jl documentation",
    "title": "PtFEM.p41",
    "category": "Function",
    "text": "Method p41\n\nOne dimensional analysis of an axially loaded elastic Rod using 2-node  Line elements. \n\nConstructors\n\np41(data)\n\nArguments\n\n* `data::Dict{Symbol, Any}`  : Dictionary containing all input data\n\nRequired data dictionary keys\n\n* struc_el::StructuralElement                          : Type of  structural fin_el\n* support::Array{Tuple{Int64,Array{Int64,1}},1}        : Fixed-displacements vector\n* loaded_nodes::Array{Tuple{Int64,Array{Float64,1}},1} : Node load vector\n* properties::Vector{Float64}                          : Material properties\n* x_coords::FloatRange{Float64}                        : x-coordinate vector\n\nOptional additional data dictionary keys\n\n* penalty = 1e20               : Penalty used for fixed degrees of freedoms\n* etype::Vector{Int64}         : Element material vector if np_types > 1\n* eq_nodal_forces_and_moments  : Contribution of distributed loads to loaded_nodes\n\nReturn values\n\n* (jfem, dis_dt, fm_dt)        : Tuple of jFem, dis_dt and fm_dt\n                                 where:\n                                    jfem::jFem    : Computational result type\n                                    dis_dt        : Displacement data table\n                                    fm_dt         : Forces and moments data table\n\nRelated help\n\n?StructuralElement             : List of available structural element types\n?Rod                           : Help on a Rod structural element\n?FiniteElement                 : List finite element types\n?Line                          : Help on Line finite element\n\n\n\nMethod p41\n\nOne dimensional analysis of an axially loaded elastic Rod using 2-node  Line elements. \n\nConstructors\n\np41(m, data) # Re-use factored global stiffness matrix\n\nArguments\n\n* `m::jFEM`                  : Previously created jFEM model\n* `data::Dict{Symbol, Any}`  : Dictionary containing all input data\n\nRequired data dictionary keys\n\n* struc_el::StructuralElement                          : Type of  structural fin_el\n* support::Array{Tuple{Int64,Array{Int64,1}},1}        : Fixed-displacements vector\n* loaded_nodes::Array{Tuple{Int64,Array{Float64,1}},1} : Node load vector\n* properties::Vector{Float64}                          : Material properties\n* x_coords::FloatRange{Float64}                        : x-coordinate vector\n\nOptional additional data dictionary keys\n\n* penalty = 1e20               : Penalty used for fixed degrees of freedoms\n* etype::Vector{Int64}         : Element material vector if np_types > 1\n* eq_nodal_forces_and_moments  : Contribution of distributed loads to loaded_nodes\n\nReturn values\n\n* (jfem, dis_dt, fm_dt)        : Tuple of jFem, dis_dt and fm_dt\n                                 where:\n                                    jfem::jFem    : Computational result type\n                                    dis_dt        : Displacement data table\n                                    fm_dt         : Forces and moments data table\n\nRelated help\n\n?StructuralElement             : List of available structural element types\n?Rod                           : Help on a Rod structural element\n?FiniteElement                 : List finite element types\n?Line                          : Help on Line finite element\n\n\n\n"
},

{
    "location": "index.html#PtFEM.p42-Tuple{Dict{Symbol,Any}}",
    "page": "PtFEM.jl documentation",
    "title": "PtFEM.p42",
    "category": "Method",
    "text": "Method p42\n\nAnalysis of elastic pin-jointed frames using 2-node rod elements in 2- or 3-dimensions.\n\nConstructors\n\np42(data)\n\nArguments\n\n* `data::Dict{Symbol, Any}`  : Dictionary containing all input data\n\nDictionary keys\n\n* struc_el::StructuralElement                          : Type of structural element\n* support::Array{Tuple{Int64,Array{Int64,1}},1}        : Fixed-displacements vector\n* loaded_nodes::Array{Tuple{Int64,Array{Float64,1}},1} : Node load vector\n* properties::Vector{Float64}                          : Material properties\n* x_coords::Vector{Float64}                            : x coordinate vector\n* y_coords::Vector{Float64}                            : y coordinate vector\n* g_num::Array{Int64,2}                                : Element node connections\n\nOptional additional dictionary keys\n\n* penalty::Float64             : Penalty for fixed freedoms\n* etype::Vector{Int64}         : Element material vector\n* z_coords::Vector{Float64}    : z coordinate vector (3D)\n* eq_nodal_forces_and_moments  : Contribution of distributed loads to loaded_nodes\n\nReturn values\n\n* (jfem, dis_dt, fm_dt)        : Tuple of jFem, dis_dt and fm_dt\n                                 where:\n                                    jfem::jFem    : Computational result type\n                                    dis_dt        : Displacement data table\n                                    fm_dt         : Forces and moments data table\n\nRelated help\n\n?StructuralElement  : List structural element types\n?Frame              : Help on a Rod structural fin_el\n?FiniteElement      : List finite element types\n?Line               : Help on Line finite element\n\n\n\n"
},

{
    "location": "index.html#PtFEM.p43-Tuple{Dict{Symbol,Any}}",
    "page": "PtFEM.jl documentation",
    "title": "PtFEM.p43",
    "category": "Method",
    "text": "p43\n\nAnalysis of elastic beams using 2-node Beam structural elements and Line finite elements. Elastic foundation is optional.\n\nConstructors\n\np43(data)\n\nArguments\n\n* `data::Dict{Symbol, Any}` : Dictionary containing all input data\n\nDictionary keys\n\n* struc_el::StructuralElement                          : Type of  structural fin_el\n* support::Array{Tuple{Int64,Array{Int64,1}},1}        : Fixed-displacements vector\n* loaded_nodes::Array{Tuple{Int64,Array{Float64,1}},1} : Node load vector\n* properties::Vector{Float64}                          : Material properties\n* x_coords::LinSpace{Float64}                          : x coordinate vector\n* g_num::Array{Int64,2}                                : Element node connections\n* fixed_freedoms::Array{Tuple{Vector{Int64}}           : Fixed freedoms\n\nOptional additional dictionary keys\n\n* etype::Vector{Int64}                                 : Element material vector\n* penalty::Float64                                     : Penalty for fixed freedoms\n* eq_nodal_forces_and_moments                          : Equivalent nodal loads\n\nReturn values\n\n* (jfem, dis_dt, fm_dt)        : Tuple of jFem, dis_dt and fm_dt\n                                 where:\n                                    jfem::jFem    : Computational result type\n                                    dis_dt        : Displacement data table\n                                    fm_dt         : Forces and moments data table\n\nRelated help\n\n?StructuralElement  : Help on structural elements\n?Rod                : Help on a Rod structural fin_el\n?FiniteElement      : Help on finite element types\n\n\n\n"
},

{
    "location": "index.html#PtFEM.p44-Tuple{Dict{Symbol,Any}}",
    "page": "PtFEM.jl documentation",
    "title": "PtFEM.p44",
    "category": "Method",
    "text": "p44\n\nAnalysis of elastic rigid-joonted frames using a 2-node Frame structural element and Line finite elements in 2 or 3 dimensions.\n\nConstructors\n\np44(data)\n\nArguments\n\n* `data::Dict{Symbol, Any}` : Dictionary containing all input data\n\nDictionary keys\n\n* struc_el::StructuralElement                          : Type of  structural fin_el\n* support::Array{Tuple{Int64,Array{Int64,1}},1}        : Fixed-displacements vector\n* loaded_nodes::Array{Tuple{Int64,Array{Float64,1}},1} : Node load vector\n* properties::Vector{Float64}                          : Material properties\n* x_coords::FloatRange{Float64}                        : x coordinate vector\n* y_coords::FloatRange{Float64}                        : y coordinate vector\n* g_num::Array{Int64,2}                                : Element node connections\n* fixed_freedoms::Array{Tuple{Vector{Int64}}           : Fixed freedoms\n\nOptional additional dictionary keys\n\n* etype::Vector{Int64}                                 : Element material vector\n* penalty::Float64                                     : Penalty for fixed freedoms\n* z_coords::FloatRange{Float64}                        : z coordinate vector\n* eq_nodal_forces_and_moments                          : Equivalent nodal loads\n\nRelated help\n\n?StructuralElement  : Help on structural elements\n?Beam               : Help on a Beam structural fin_el\n?FiniteElement      : Help on finite element types\n\n\n\n"
},

{
    "location": "index.html#PtFEM.p45-Tuple{Dict{Symbol,Any}}",
    "page": "PtFEM.jl documentation",
    "title": "PtFEM.p45",
    "category": "Method",
    "text": "Method p45\n\nAnalysis of elasto-plastic beams or rigid-jointed frames using a 2-node Frame structural element in 1, 2 or 3 dimensions. \n\nConstructors\n\np45(data)\n\nArguments\n\n* `data::Dict{Symbol, Any}`  : Dictionary containing all input data\n\nRequired data dictionary keys\n\n* struc_el::StructuralElement                          : Type of  structural element\n* support::Array{Tuple{Int64,Array{Int64,1}},1}        : Fixed-displacements vector\n* loaded_nodes::Array{Tuple{Int64,Array{Float64,1}},1} : Node load vector\n* properties::Vector{Float64}                          : Material properties\n* x_coords::FloatRange{Float64}                        : x-coordinate vector\n* dload::FloatRange{Float64}                           : load steps\n\nOptional additional data dictionary keys\n\n* penalty = 1e20                 : Penalty used for fixed degrees of freedoms\n* etype::Vector{Int64}           : Element material vector if np_types > 1\n* y_coords::FloatRange{Float64}  : y-coordinate vector (2D)\n* z_coords::FloatRange{Float64}  : x-coordinate vector (3D)\n* limit = 250                    : Iteration limit\n* tol = 0.0001                   : Tolerance for iteration convergence\n\nRelated help\n\n?StructuralElement             : List of available structural element types\n?Frame                         : Help on a Frame structural element\n?FiniteElement                 : List finite element types\n?Line                          : Help on Line finite element\n\n\n\n"
},

{
    "location": "index.html#PtFEM.p46-Tuple{Dict{Symbol,Any}}",
    "page": "PtFEM.jl documentation",
    "title": "PtFEM.p46",
    "category": "Method",
    "text": "Method p46\n\nStability (buckling) analysis of elastic beams using a 2-node Beam structural element and Line finite elements. Elastic foundation is optional.\n\nConstructors\n\np46(data)\n\nArguments\n\n* `data::Dict{Symbol, Any}` : Dictionary containing all input data\n\nRequired data dictionary keys\n\n* struc_el::StructuralElement                          : Type of  structural fin_el\n* support::Array{Tuple{Int64,Array{Int64,1}},1}        : Fixed-displacements vector\n* properties::Vector{Float64}                          : Material properties\n* x_coords::FloatRange{Float64}                        : x-coordinate vector\n\nOptional additional data dictionary keys\n\n* etype::Vector{Int64}         : Element material vector if np_types > 1\n* limit = 250                  : Iteration limit\n* tol = 0.0001                 : Tolerance for iteration convergence\n\nRelated help\n\n?StructuralElement             : List of available structural element types\n?Beam                          : Help on a Beam structural element\n?FiniteElement                 : List finite element types\n?Line                          : Help on Line finite element\n\n\n\n"
},

{
    "location": "index.html#PtFEM.p47-Tuple{Dict{Symbol,Any}}",
    "page": "PtFEM.jl documentation",
    "title": "PtFEM.p47",
    "category": "Method",
    "text": "Method p47\n\nAnalysis of plates (Plne structural element) using 4-node Quadrilateral finite elements. Homogeneous material with identical elements. Mesh numbered in x or y direction.\n\nConstructors\n\np47(data)\n\nArguments\n\n* `data::Dict{Symbol, Any}` : Dictionary containing all input data\n\nRequired data dictionary keys\n\n* struc_el::StructuralElement                          : Structural element\n* support::Array{Tuple{Int64,Array{Int64,1}},1}        : Fixed-displacements vector\n* loaded_nodes::Array{Tuple{Int64,Array{Float64,1}},1} : Node load vector\n* properties::Vector{Float64}                          : Material properties\n* x_coords::FloatRange{Floalt64}                       : x-coordinate vector\n* y_coords::FloatRange{Floalt64}                       : y-coordinate vector\n* thickness:: Float64                                  : Thickness of plate\n\nOptional additional data dictionary keys\n\n* penalty = 1e20               : Penalty used for fixed degrees of freedoms\n* etype::Vector{Int64}         : Element material vector if np_types > 1\n\nReturn values\n\n* (fm_dt, sigma_dt)            : Tuple of jFem, dis_dt and fm_dt\n                                  where:\n                                    fm_dt         : Forces and moments data table\n                                    sigma_dt      : Stresses data table\n\nRelated help\n\n?StructuralElement             : List of available structural element types\n?Plane                         : Help on a Plane structural element\n?FiniteElement                 : List finite element types\n?Quadrilateral                 : Help on Quadrilateral finite element\n\n\n\n"
},

{
    "location": "index.html#Static-Equilibrium-Programs-1",
    "page": "PtFEM.jl documentation",
    "title": "4 Static Equilibrium Programs",
    "category": "section",
    "text": "p41\np42(data::Dict{Symbol, Any})\np43(data::Dict{Symbol, Any})\np44(data::Dict{Symbol, Any})\np45(data::Dict{Symbol, Any})\np46(data::Dict{Symbol, Any})\np47(data::Dict{Symbol, Any})"
},

{
    "location": "index.html#Elastic-Solids-Programs-1",
    "page": "PtFEM.jl documentation",
    "title": "5 Elastic Solids Programs",
    "category": "section",
    "text": "p51(data::Dict{Symbol, Any})\np52(data::Dict{Symbol, Any})\np53(data::Dict{Symbol, Any})\np54(data::Dict{Symbol, Any})\np55(data::Dict{Symbol, Any})\np56(data::Dict{Symbol, Any})"
},

{
    "location": "index.html#Material-Nonlinearity-Programs-1",
    "page": "PtFEM.jl documentation",
    "title": "6 Material Nonlinearity Programs",
    "category": "section",
    "text": "p61(data::Dict{Symbol, Any})\np62(data::Dict{Symbol, Any})"
},

{
    "location": "index.html#Structural-Element-Types-1",
    "page": "PtFEM.jl documentation",
    "title": "Structural Element Types",
    "category": "section",
    "text": "StructuralElement\nRod\nBeam\nFrame\nPlane\nSolid\nGenericSolid"
},

{
    "location": "index.html#PtFEM.FiniteElement",
    "page": "PtFEM.jl documentation",
    "title": "PtFEM.FiniteElement",
    "category": "Type",
    "text": "FiniteElement\n\nAbstract finite element type.\n\nType\n\nabstract FiniteElement\n\nSubtypes\n\n* Line::FiniteElement          : 1D Line(nod, nodof)\n* Triangle::FiniteElement      : 2D Triangle(nod, nodof)\n* Quadrilateral::FiniteElement : 2D Quadrilateral(nod, nodof)\n* Hexahedron::FiniteElement    : 3D Hexahedron(nod, nodof)\n* Tetrahedron::FiniteElement   : 3D Tetrahedron(nod, nodof)\n\n\n\n"
},

{
    "location": "index.html#PtFEM.Line",
    "page": "PtFEM.jl documentation",
    "title": "PtFEM.Line",
    "category": "Type",
    "text": "Line (Interval)\n\n1D type finite element\n\nConstructor\n\nLine(nod, nodof)\nLine(nodof)\n\nArguments\n\n* nod::Int64       : Number of nodes for finite element, defaults to 2\n* nodof::Int64     : Number of degrees of freedom per node\n\nRelated help\n\n?FiniteElement      : Help on finite element types\n\n\n\n"
},

{
    "location": "index.html#PtFEM.Triangle",
    "page": "PtFEM.jl documentation",
    "title": "PtFEM.Triangle",
    "category": "Type",
    "text": "Triangle\n\n2D type finite element\n\nConstructor\n\nTriangle(nod, nodof)\n\nArguments\n\n* nod::Int64       : Number of nodes for finite element (3, 6, 10, 15)\n* nodof::Int64     : Number of degrees of freedom per node\n\nRelated help\n\n?FiniteElement      : Help on finite element types\n\n\n\n"
},

{
    "location": "index.html#PtFEM.Quadrilateral",
    "page": "PtFEM.jl documentation",
    "title": "PtFEM.Quadrilateral",
    "category": "Type",
    "text": "Quadrilateral\n\n2D type finite element\n\nConstructor\n\nQuadrilateral(nod, nodof)\n\nArguments\n\n* nod::Int64       : Number of nodes for finite element (4, 8, 9)\n* nodof::Int64     : Number of degrees of freedom per node\n\nRelated help\n\n?FiniteElement      : Help on finite element types\n\n\n\n"
},

{
    "location": "index.html#PtFEM.Hexahedron",
    "page": "PtFEM.jl documentation",
    "title": "PtFEM.Hexahedron",
    "category": "Type",
    "text": "hexahedron\n\n3D type finite element\n\nConstructor\n\nHexahedron(nod, nodof)\n\nArguments\n\n* nod::Int64       : Number of nodes for finite element (8, 14, 20)\n* nodof::Int64     : Number of degrees of freedom per node\n\nRelated help\n\n?FiniteElement      : Help on finite element types\n\n\n\n"
},

{
    "location": "index.html#PtFEM.Tetrahedron",
    "page": "PtFEM.jl documentation",
    "title": "PtFEM.Tetrahedron",
    "category": "Type",
    "text": "Tetrahedron\n\n3D type finite element\n\nConstructor\n\nTetrahedron(nod, nodof)\nTetrahedron(nodof)\n\nArguments\n\n* nod::Int64       : Number of nodes for finite element (defaults to 4)\n* nodof::Int64     : Number of degrees of freedom per node\n\nRelated help\n\n?FiniteElement      : Help on finite element types\n\n\n\n"
},

{
    "location": "index.html#Finite-Element-Types-1",
    "page": "PtFEM.jl documentation",
    "title": "Finite Element Types",
    "category": "section",
    "text": "FiniteElement\nLine\nTriangle\nQuadrilateral\nHexahedron\nTetrahedron"
},

{
    "location": "index.html#Other-Julia-Types-1",
    "page": "PtFEM.jl documentation",
    "title": "Other Julia Types",
    "category": "section",
    "text": "FEM\njFEM"
},

{
    "location": "index.html#PtFEM-Main-1",
    "page": "PtFEM.jl documentation",
    "title": "PtFEM - Main",
    "category": "section",
    "text": "PtFEM.beam_gm!(gm::Matrix{Float64}, ell::Float64)\nPtFEM.beam_km!(km::Matrix{Float64}, ei::Float64, ell::Float64)\nPtFEM.beam_mm!(mm::Matrix{Float64}, fs::Float64, ell::Float64)\nPtFEM.beemat_nonaxi!(bee::Matrix{Float64}, radius::Float64, coord::Matrix{Float64}, deriv::Matrix{Float64}, fun::Vector{Float64}, iflag::Int64, lth::Int64)\nPtFEM.beemat!(bee::Matrix{Float64},deriv::Matrix{Float64})\nPtFEM.checon!(loads::Vector{Float64}, oldlds::Vector{Float64}, tol::Float64)\nPtFEM.deemat!(dee::Array{Float64, 2}, e::Float64, v::Float64)\nPtFEM.fkdiag!(ndof::Int64, neq::Int64, g::Vector{Int64}, kdiag::Vector{Int64})\nPtFEM.fkdiag!(kdiag::Vector{Int64}, g::Vector{Int64})\nPtFEM.fmplat!(d2x::Vector{Float64}, d2y::Vector{Float64}, d2xy::Vector{Float64}, points::Matrix{Float64}, aa::Float64, bb::Float64, i::Int64)\nPtFEM.formm!(stress::Vector{Float64}, m1::Matrix{Float64}, m2::Matrix{Float64}, m3::Matrix{Float64})\nPtFEM.formnf!(nodof::Int64, nn::Int64, nf::Matrix{Int64})\nPtFEM.glob_to_loc!(loc::Vector{Float64}, glob::Vector{Float64}, gamma::Float64, coord::Matrix{Float64})\nPtFEM.global_to_axial(glob::Vector{Float64}, coord::Matrix{Float64})\nPtFEM.hinge!(coord::Matrix{Float64}, holdr::Matrix{Float64}, action::Vector{Float64}, react::Vector{Float64}, prop::Matrix{Float64}, iel, etype::Vector{Int64}, gamma::Vector{Float64})\nPtFEM.invar!(stress::Vector{Float64}, sigm::Float64, dsbar::Float64, theta::Float64)\nPtFEM.linmul_sky!(kv::Vector{Float64}, disps::Vector{Float64}, loads::Vector{Float64}, kdiag::Vector{Int64})\nPtFEM.loc_to_glob!(loc::Vector{Float64}, glob::Vector{Float64}, gamma::Float64, coord::Matrix{Float64})\nPtFEM.num_to_g!(num::Vector{Int64}, nf::Matrix{Int64}, g::Vector{Int64})\nPtFEM.pin_jointed!(km::Matrix{Float64}, ea::Float64, coord::Matrix{Float64})\nPtFEM.rigid_jointed!(km::Matrix{Float64}, prop::Matrix{Float64}, gamma::Vector{Float64}, etype::Vector{Int64}, iel::Int64, coord::Matrix{Float64})\nPtFEM.rod_km!(km::Matrix{Float64}, ea::Float64, length::Float64)\nPtFEM.format_nf_line!(l::AbstractString, t::Array{Tuple})\nPtFEM.read_nf_file(f::AbstractString)\nPtFEM.format_loads_line!(l::AbstractString, t::Array{Tuple})\nPtFEM.read_loads_file(f::AbstractString)\nPtFEM.rod_mm!(mm::Matrix{Float64}, length::Float64)\nPtFEM.sample!(fin_el::Hexahedron, s::Matrix{Float64} , wt::Vector{Float64})\nPtFEM.sample!(fin_el::Line, s::Matrix{Float64} , wt::Vector{Float64})\nPtFEM.sample!(fin_el::Quadrilateral, s::Matrix{Float64} , wt::Vector{Float64})\nPtFEM.sample!(fin_el::Tetrahedron, s::Matrix{Float64} , wt::Vector{Float64})\nPtFEM.sample!(fin_el::Triangle, s::Matrix{Float64} , wt::Vector{Float64})\nPtFEM.shape_der!(der::Matrix{Float64}, points::Matrix{Float64}, i::Int64)\nPtFEM.shape_fun!(fun::Vector{Float64}, points::Matrix{Float64}, i::Int64)\nPtFEM.stability(gsm::SparseMatrixCSC{Float64,Int64}, ggm::SparseMatrixCSC{Float64,Int64}, tol::Float64, limit::Int64)"
},

{
    "location": "index.html#PtFEM-Geom-1",
    "page": "PtFEM.jl documentation",
    "title": "PtFEM - Geom",
    "category": "section",
    "text": "PtFEM.geom_rect!(fin_el::Quadrilateral, iel::Int64, x_coords::Array{Float64, 1}, y_coords::Array{Float64, 1}, coord::Matrix{Float64}, num::Vector{Int64}, dir::Symbol)\nPtFEM.geom_rect!(fin_el::Triangle, iel::Int64, x_coords::Array{Float64, 1}, y_coords::Array{Float64, 1}, coord::Matrix{Float64}, num::Vector{Int64}, dir::Symbol)\nhexahedron_xz!(iel::Int64, x_coords::Vector{Float64}, y_coords::Vector{Float64}, z_coords::Vector{Float64}, coord::Matrix{Float64}, num::Vector{Int64})\nPtFEM.mesh_size(fe::Line, nxe::Int64)\nPtFEM.mesh_size(fe::Triangle, nxe::Int64, nye::Int64)\nPtFEM.mesh_size(fe::Quadrilateral, nxe::Int64, nye::Int64)\nPtFEM.mesh_size(fe::Hexahedron, nxe::Int64, nye::Int64, nze::Int64)"
},

{
    "location": "index.html#PtFEM.mesh-Tuple{Dict,Array{Float64,2},Array{Int64,2},Any,Any,Any}",
    "page": "PtFEM.jl documentation",
    "title": "PtFEM.mesh",
    "category": "Method",
    "text": "mesh\n\nPlots displacements and directions\n\nFunction\n\nmesh(data, g_coord, g_num, disp, ampl, pdir)\n\nArguments\n\n* data::Dict                 : Input dictionary\n* g_coord::Array{Float64, 2} : Coordinate array\n* g_num::Array{Int, 2}       : Global node numbering array\n* disp::DataTable            : Displacements DataTable\n* ampl::Float64              : Amplification for derivatives\n* pdir::AbstractString       : Nodal freedom matrix (updated)\n\n\n\n"
},

{
    "location": "index.html#PtFEM-Plot-methods-1",
    "page": "PtFEM.jl documentation",
    "title": "PtFEM - Plot methods",
    "category": "section",
    "text": "PtFEM.mesh(data::Dict, g_coord::Array{Float64,2}, g_num::Array{Int, 2}, disp, ampl, pdir)"
},

{
    "location": "index.html#PtFEM-VTK-methods-1",
    "page": "PtFEM.jl documentation",
    "title": "PtFEM - VTK methods",
    "category": "section",
    "text": "PtFEM.vtk(data::Dict, fm_dt, sigma_dt, dir, fname)"
},

{
    "location": "index.html#Base.LinAlg.cholfact",
    "page": "PtFEM.jl documentation",
    "title": "Base.LinAlg.cholfact",
    "category": "Function",
    "text": "cholfact(A, [uplo::Symbol,] Val{false}) -> Cholesky\n\nCompute the Cholesky factorization of a dense symmetric positive definite matrix A and return a Cholesky factorization. The matrix A can either be a Symmetric or Hermitian StridedMatrix or a perfectly symmetric or Hermitian StridedMatrix. In the latter case, the optional argument uplo may be :L for using the lower part or :U for the upper part of A. The default is to use :U. The triangular Cholesky factor can be obtained from the factorization F with: F[:L] and F[:U]. The following functions are available for Cholesky objects: size, \\, inv, det. A PosDefException exception is thrown in case the matrix is not positive definite.\n\n\n\ncholfact(A, [uplo::Symbol,] Val{true}; tol = 0.0) -> CholeskyPivoted\n\nCompute the pivoted Cholesky factorization of a dense symmetric positive semi-definite matrix A and return a CholeskyPivoted factorization. The matrix A can either be a Symmetric or Hermitian StridedMatrix or a perfectly symmetric or Hermitian StridedMatrix. In the latter case, the optional argument uplo may be :L for using the lower part or :U for the upper part of A. The default is to use :U. The triangular Cholesky factor can be obtained from the factorization F with: F[:L] and F[:U]. The following functions are available for PivotedCholesky objects: size, \\, inv, det, and rank. The argument tol determines the tolerance for determining the rank. For negative values, the tolerance is the machine precision.\n\n\n\ncholfact(A; shift = 0.0, perm = Int[]) -> CHOLMOD.Factor\n\nCompute the Cholesky factorization of a sparse positive definite matrix A. A must be a SparseMatrixCSC, Symmetric{SparseMatrixCSC}, or Hermitian{SparseMatrixCSC}. Note that even if A doesn't have the type tag, it must still be symmetric or Hermitian. A fill-reducing permutation is used. F = cholfact(A) is most frequently used to solve systems of equations with F\\b, but also the methods diag, det, logdet are defined for F. You can also extract individual factors from F, using F[:L]. However, since pivoting is on by default, the factorization is internally represented as A == P'*L*L'*P with a permutation matrix P; using just L without accounting for P will give incorrect answers. To include the effects of permutation, it's typically preferable to extact \"combined\" factors like PtL = F[:PtL] (the equivalent of P'*L) and LtP = F[:UP] (the equivalent of L'*P).\n\nSetting optional shift keyword argument computes the factorization of A+shift*I instead of A. If the perm argument is nonempty, it should be a permutation of 1:size(A,1) giving the ordering to use (instead of CHOLMOD's default AMD ordering).\n\nnote: Note\nThis method uses the CHOLMOD library from SuiteSparse, which only supports doubles or complex doubles. Input matrices not of those element types will be converted to SparseMatrixCSC{Float64} or SparseMatrixCSC{Complex128} as appropriate.Many other functions from CHOLMOD are wrapped but not exported from the Base.SparseArrays.CHOLMOD module.\n\n\n\n"
},

{
    "location": "index.html#Base.:\\",
    "page": "PtFEM.jl documentation",
    "title": "Base.:\\",
    "category": "Function",
    "text": "\\(x, y)\n\nLeft division operator: multiplication of y by the inverse of x on the left. Gives floating-point results for integer arguments.\n\n\n\n\\(A, B)\n\nMatrix division using a polyalgorithm. For input matrices A and B, the result X is such that A*X == B when A is square. The solver that is used depends upon the structure of A.  If A is upper or lower triangular (or diagonal), no factorization of A is required and the system is solved with either forward or backward substitution. For non-triangular square matrices, an LU factorization is used.\n\nFor rectangular A the result is the minimum-norm least squares solution computed by a pivoted QR factorization of A and a rank estimate of A based on the R factor.\n\nWhen A is sparse, a similar polyalgorithm is used. For indefinite matrices, the LDLt factorization does not use pivoting during the numerical factorization and therefore the procedure can fail even for invertible matrices.\n\n\n\n"
},

{
    "location": "index.html#PtFEM-Julia-functions-and-operators-1",
    "page": "PtFEM.jl documentation",
    "title": "PtFEM - Julia functions & operators",
    "category": "section",
    "text": "cholfact\n\\"
},

{
    "location": "index.html#Base.pmap",
    "page": "PtFEM.jl documentation",
    "title": "Base.pmap",
    "category": "Function",
    "text": "pmap([::AbstractWorkerPool], f, c...; distributed=true, batch_size=1, on_error=nothing, retry_n=0, retry_max_delay=DEFAULT_RETRY_MAX_DELAY, retry_on=DEFAULT_RETRY_ON) -> collection\n\nTransform collection c by applying f to each element using available workers and tasks.\n\nFor multiple collection arguments, apply f elementwise.\n\nNote that f must be made available to all worker processes; see Code Availability and Loading Packages for details.\n\nIf a worker pool is not specified, all available workers, i.e., the default worker pool is used.\n\nBy default, pmap distributes the computation over all specified workers. To use only the local process and distribute over tasks, specify distributed=false. This is equivalent to asyncmap.\n\npmap can also use a mix of processes and tasks via the batch_size argument. For batch sizes greater than 1, the collection is split into multiple batches, which are distributed across workers. Each such batch is processed in parallel via tasks in each worker. The specified batch_size is an upper limit, the actual size of batches may be smaller and is calculated depending on the number of workers available and length of the collection.\n\nAny error stops pmap from processing the remainder of the collection. To override this behavior you can specify an error handling function via argument on_error which takes in a single argument, i.e., the exception. The function can stop the processing by rethrowing the error, or, to continue, return any value which is then returned inline with the results to the caller.\n\nFailed computation can also be retried via retry_on, retry_n, retry_max_delay, which are passed through to retry as arguments retry_on, n and max_delay respectively. If batching is specified, and an entire batch fails, all items in the batch are retried.\n\nThe following are equivalent:\n\npmap(f, c; distributed=false) and asyncmap(f,c)\npmap(f, c; retry_n=1) and asyncmap(retry(remote(f)),c)\npmap(f, c; retry_n=1, on_error=e->e) and asyncmap(x->try retry(remote(f))(x) catch e; e end, c)\n\n\n\n"
},

{
    "location": "index.html#PtFEM-Parallel-processing-1",
    "page": "PtFEM.jl documentation",
    "title": "PtFEM - Parallel processing",
    "category": "section",
    "text": "pmap"
},

{
    "location": "index.html#PtFEM-No-longer-used-1",
    "page": "PtFEM.jl documentation",
    "title": "PtFEM - No longer used",
    "category": "section",
    "text": "PtFEM.sparin!(kv::Vector{Float64}, kdiag::Vector{Int64})\nPtFEM.spabac!(kv::Vector{Float64}, loads::Vector{Float64}, kdiag::Vector{Int64})\nfromSkyline(skyline::Vector{Float64}, kdiag::Vector{Int64})\nPtFEM.skyline2sparse(skyline::Vector{Float64}, kdiag::Vector{Int64})\nPtFEM.stability!(kv::Vector{Float64}, gv::Vector{Float64}, kdiag::Vector{Int64}, tol::Float64, limit::Int64, iters::Int64, evec::Vector{Float64}, ival::Float64)"
},

{
    "location": "index.html#Index-1",
    "page": "PtFEM.jl documentation",
    "title": "Index",
    "category": "section",
    "text": ""
},

{
    "location": "VERSIONS.html#",
    "page": "Versions",
    "title": "Versions",
    "category": "page",
    "text": ""
},

{
    "location": "VERSIONS.html#Version-approach-and-history-1",
    "page": "Versions",
    "title": "Version approach and history",
    "category": "section",
    "text": ""
},

{
    "location": "VERSIONS.html#Approach-1",
    "page": "Versions",
    "title": "Approach",
    "category": "section",
    "text": "A version of a Julia package is labeled (tagged) as v\"major.minor.patch\".My intention is to update the patch level whenever I make updates to programs which are not visible to the then existing examples. This also includes adding new chapters and examples.Changes that require updates to some examples bump the minor level.Updates for new releases of Julia bump the major level."
},

{
    "location": "VERSIONS.html#Version-history-1",
    "page": "Versions",
    "title": "Version history",
    "category": "section",
    "text": ""
},

{
    "location": "VERSIONS.html#v\"0.0.1\"-1",
    "page": "Versions",
    "title": "v\"0.0.1\"",
    "category": "section",
    "text": "Initial release of PtFEM/PtFEM.jl. Contains the programs in chapters 4, 5 and the first 2 programs of chapter 6."
},

{
    "location": "TODO.html#",
    "page": "Todo",
    "title": "Todo",
    "category": "page",
    "text": ""
},

{
    "location": "TODO.html#TODO-list-1",
    "page": "Todo",
    "title": "TODO list",
    "category": "section",
    "text": ""
},

{
    "location": "TODO.html#Planned-work-1",
    "page": "Todo",
    "title": "Planned work",
    "category": "section",
    "text": "Complete inital framework, including documentation (May 2017)\nRemaining programs in chapter 6 (June 2017)\nReview and complete plot recipes for chapters 4 to 6 (June 2017)\nRework chapters 5 and 6 for Julia sparse matrices (May 2017)\nUpdate notebooks (July 2017)\nAdd generalized WriteVTK.jl recipes\nProfile p5. and p6. programs (summer 2017?)\nComplete chapter 7\nComplete chapter 8 (Sep 2017)\nComplete chapter 9 (fall 2017?)\nComplete chapter 10\nComplete chapter 11\nComplete chapter 12 (late 2017?)\nPort BHATheoreticalPerformance from Fortran/R/Julia to using PtFEM.jl  (early 2018?)\nPort BHALockup from Fortran/R to using PtFEM.jl (mid 2018?)"
},

{
    "location": "TODO.html#Intended-extensions-but-not-yet-planned-1",
    "page": "Todo",
    "title": "Intended extensions but not yet planned",
    "category": "section",
    "text": "Add thin shell model to compare results with the beam model\nAdd composite (thin shell?) models\nTemperature and pressure considerations\nAdd statistical modeling components\nGeomechanics applications, e.g. bore hole stability"
},

{
    "location": "REFERENCES.html#",
    "page": "References",
    "title": "References",
    "category": "page",
    "text": ""
},

{
    "location": "REFERENCES.html#References-1",
    "page": "References",
    "title": "References",
    "category": "section",
    "text": "Programming the Finite Element Method\nPtFEM.jl\ndeal.II\nFEniCS\nJuaFEM.jl\nJulia language\nApproxFun.jl\nDifferentialEquations.jl\nJuliaFEM.jl\nNumerical Methods for Engineers\nNMfE.jl\nSymata.jl\nPlots.jl"
},

]}
