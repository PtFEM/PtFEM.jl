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
    "text": "This Julia package will contain the programs in \"Programming the Finite Element Method\" by I M Smith, D V Griffiths and L. Margetts (PtFEM). See TODO for the planned progress.PtFEM is a very versatile toolkit to construct FEM programs for practical engineering and scientific problems (to distinguish it somewhat from systems primarily focused on solving symbolic partial differential equations). Each chapter in the book gradually develops a set of related programs intended to be used as a starting point for a particular type of problem.I use PtFEM when referring to the book and PtFEM.jl when referring to the Julia package. PtFEM.jl is the central package in a mini-PtFEM-ecosystem and most other packages will be using PtFEM.jl as the starting point."
},

{
    "location": "INTRO.html#A-Julia-based-PtFEM-eco-system-1",
    "page": "Introduction",
    "title": "A Julia based PtFEM eco system",
    "category": "section",
    "text": "For many years a colleague and I have used the Fortran version of the PtFEM toolkit  to verify aspects of a (larger) software program developed to analyse the behavior of bottom hole assemblies (BHAs). PtFEM has been extremely valuable for that purpose and that alone in my opinion justifies the creation of PtFEM.jl.We are now at a point where we would like to publish the results of our work in a reproducable (and maintainable) format. Thus an important secundairy motivation for creating PtFEM.jl is to be able to subsequently publish 2 additional packages, BHATheoreticalPerformance.jl and BHALockup.jl, on the basis of a well documented toolkit.The goal of the PtFEM eco system is to not only make our results easily reproducable but also the usage of PtFEM extensible, e.g. see the examples towards the end of  TODO. If there is long term interest to contribute to the PtFEM organisation, please consider becoming a team member or outside collaborator.The PtFEM eco system exists as a Github organization. A GitHub organisation is basically a place to collect a set of related packages, in this case Julia packages around the \"Programming the Finite Element Method\" toolkit.Occasionally a package might be temporarily selfstanding, e.g. ClassicalLaminateTheory.jl at present. The intention is to in the future upgrade/extend/incorporate that package, e.g. in the case of ClassicalLaminateTheory.jl to handle composite Bottom Hole Assembly modeling or extended reach casing installation.A companion package to PtFEM, NMfE.jl along the lines of Numerical Methods for Engineers by I M Smith and D V Griffiths, is also included in the PtFEM organisation but development work for this educational set of programs is happening on a slower pace because:In my mind it should form a bridge to some of the Julia packages mentioned in the README under related packages and focused on solving differential equations\nIt is less directly derived from the original Fortran programs, e.g. the symbolic examples in chapter 7 of NMfE using the Julia Symata package."
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
    "page": "PtFEM.jl programmer's documentation",
    "title": "PtFEM.jl programmer's documentation",
    "category": "page",
    "text": ""
},

{
    "location": "index.html#PtFEM.jl-Documentation-1",
    "page": "PtFEM.jl programmer's documentation",
    "title": "PtFEM.jl Documentation",
    "category": "section",
    "text": ""
},

{
    "location": "index.html#PtFEM.p41-Tuple{Dict{Symbol,Any}}",
    "page": "PtFEM.jl programmer's documentation",
    "title": "PtFEM.p41",
    "category": "Method",
    "text": "Method p41\n\nOne dimensional analysis of axially loaded elastic rods using 2-node rod elements. \n\nConstructors\n\np41(data::Dict)\np41(m::jFEM, data::Dict) # Used to re-use factored global stiffness matrix\n\nArguments\n\n* `m`    : Previously created jFEM model\n* `data` : Dictionary containing all input data\n\nRequired data dictionary keys\n\n* struc_el::StructuralElement                          : Type of  structural fin_el\n* support::Array{Tuple{Int64,Array{Int64,1}},1}        : Fixed-displacements vector\n* loaded_nodes::Array{Tuple{Int64,Array{Float64,1}},1} : Node load vector\n* properties::Vector{Float64}                          : Material properties\n* x_coords::0.0:0.1:1.0                                : x-coordinate vector\n\nOptional additional data dictionary keys\n\n* penalty = 1e20               : Penalty used for fixed degrees of freedoms\n* etype::Vector{Int64}         : Element material vector if np_types > 1\n* eq_nodal_forces_and_moments  : Contribution of distributed loads to loaded_nodes\n\nReturn values\n\n* (jfem, dis_dt, fm_dt)        : Tuple of jFem, dis_dt and fm_dt\n                                 where:\n                                    jfem::jFem    : Computational result type\n                                    dis_dt        : Displacement data table\n                                    fm_dt         : Forces and moments data table\n\nRelated help\n\n?StructuralElement             : List of available structural element types\n?Rod                           : Help on a Rod structural element\n?FiniteElement                 : List finite element types\n?Line                          : Help on Line finite element\n\n\n\n"
},

{
    "location": "index.html#PtFEM.p42-Tuple{Dict{Symbol,Any}}",
    "page": "PtFEM.jl programmer's documentation",
    "title": "PtFEM.p42",
    "category": "Method",
    "text": "Method p42\n\nAnalysis of elastic pin-jointed frames using 2-node rod elements in 2- or 3-dimensions \n\nConstructors\n\np42(data::Dict)\n\nArguments\n\n* `data` : Dictionary containing all input data\n\nDictionary keys\n\n* struc_el::StructuralElement                          : Type of structural element\n* support::Array{Tuple{Int64,Array{Int64,1}},1}        : Fixed-displacements vector\n* loaded_nodes::Array{Tuple{Int64,Array{Float64,1}},1} : Node load vector\n* properties::Vector{Float64}                          : Material properties\n* x_coords::Vector{Float64}                            : x coordinate vector\n* y_coords::Vector{Float64}                            : y coordinate vector\n* g_num::Array{Int64,2}                                : Element node connections\n\nOptional dictionary keys\n\n* penalty::Float64             : Penalty for fixed freedoms\n* etype::Vector{Int64}         : Element material vector\n* z_coords::Vector{Float64}    : z coordinate vector (3D)\n* eq_nodal_forces_and_moments  : Contribution of distributed loads to loaded_nodes\n\nReturn values\n\n* (jfem, dis_dt, fm_dt)        : Tuple of jFem, dis_dt and fm_dt\n                                 where:\n                                    jfem::jFem    : Computational result type\n                                    dis_dt        : Displacement data table\n                                    fm_dt         : Forces and moments data table\n\nRelated help\n\n?StructuralElement  : List structural element types\n?Frame              : Help on a Rod structural fin_el\n?FiniteElement      : List finite element types\n?Line               : Help on Line finite element\n\n\n\n"
},

{
    "location": "index.html#PtFEM.p43-Tuple{Dict{Symbol,Any}}",
    "page": "PtFEM.jl programmer's documentation",
    "title": "PtFEM.p43",
    "category": "Method",
    "text": "p43\n\nMethod for static equilibrium analysis of a beam.\n\nConstructors\n\np43(data::Dict)\n\nArguments\n\n* `data` : Dictionary containing all input data\n\nDictionary keys\n\n* struc_el::StructuralElement                          : Type of  structural fin_el\n* support::Array{Tuple{Int64,Array{Int64,1}},1}        : Fixed-displacements vector\n* loaded_nodes::Array{Tuple{Int64,Array{Float64,1}},1} : Node load vector\n* properties::Vector{Float64}                          : Material properties\n* x_coords::LinSpace{Float64}                          : x coordinate vector\n* g_num::Array{Int64,2}                                : Element node connections\n* fixed_freedoms::Array{Tuple{Vector{Int64}}           : Fixed freedoms\n\nOptional dictionary keys\n\n* etype::Vector{Int64}                                 : Element material vector\n* penalty::Float64                                     : Penalty for fixed freedoms\n* eq_nodal_forces_and_moments                          : Equivalent nodal loads\n\nExamples\n\nusing PtFEM\n\ndata = Dict(\n  # Beam(ndim, nst, nxe, nip, direction, finite_element(nod, nodof), axisymmet\n  :struc_el => Beam(2, 1, 4, 1, :x, Line(2, 2), false),\n  :properties => [4.0e4; 2.0e4],\n  :etype => [1, 1, 2, 2],\n  :x_coords => [0.0, 2.5, 5.0, 8.0, 10.0],\n  :support => [\n      (1, [0 1]),\n      (4, [0 1])\n    ],\n  :loaded_nodes => [\n      (2, [-20.0 0.0]),\n      (3, [-6.0 -3.0]),\n      (4, [-8.8 2.2]),\n      (5, [-1.2 0.5333])\n    ],\n  :fixed_freedoms => [\n      (1, 2, -0.001),\n      (3, 1, -0.005)\n    ],\n  :penalty => 1e19,\n  :eq_nodal_forces_and_moments => [\n    (3, [-6.0 -3.0 -6.0 3.0]),\n    (4, [-2.8 -0.8 -1.2  0.5333])\n  ]\n)\n\nfem, dis_dt, fm_dt = p43(data)\n\nprintln(\"Displacements:\")\ndis_dt |> display\nprintln()\n\nprintln(\"Actions:\")\nfm_dt |> display\nprintln()\n\n\nRelated help\n\n?StructuralElement  : Help on structural elements\n?Rod                : Help on a Rod structural fin_el\n?FiniteElement      : Help on finite element types\n\n\n\n"
},

{
    "location": "index.html#Programs-1",
    "page": "PtFEM.jl programmer's documentation",
    "title": "Programs",
    "category": "section",
    "text": "p41(data::Dict{Symbol, Any})\np42(data::Dict{Symbol, Any})\np43(data::Dict{Symbol, Any})"
},

{
    "location": "index.html#PtFEM.StructuralElement",
    "page": "PtFEM.jl programmer's documentation",
    "title": "PtFEM.StructuralElement",
    "category": "Type",
    "text": "StructuralElement\n\nAbstract structural element type.\n\nType\n\nabstract StructuralElement\n\nSubtypes\n\n* Rod::StructuralElement          : Rod(nxe, np_types, nip, fin_el)\n* Beam::StructuralElement         : Beam(nod, nodof)\n* Frame::StructuralElement        : Frame(nod, nodof)\n* Plane::StructuralElement        : Plane(nod, nodof)\n* Solid::StructuralElement        : Solid(nod, nodof)\n* GenericSolid::StructuralElement : GenericSolid(nod, nodof)\n\nRelated help\n\n?FiniteElement                   : Show all finite elements\n?Rod                              : Help on Rod structural element\n?Beam                             : Help on Beam structural element\n?Frame                            : Help on Frame structural element\n?Plane                            : Help on Plane structural element\n?Solid                            : Help on Solid structural element\n?GenericSolid                     : Help on GenericSolid structural element\n\n\n\n"
},

{
    "location": "index.html#PtFEM.Rod",
    "page": "PtFEM.jl programmer's documentation",
    "title": "PtFEM.Rod",
    "category": "Type",
    "text": "Rod\n\nConcrete 1D structural element with only axial stresses.\n\nConstructor\n\nRod(nels, np_types, nip, fin_el)\n\nArguments\n\n* nels::Int64             : Number of fin_els (stored in field nxe)\n* np_types::Int64         : Number of different property types\n* nip::Int64              : Number of integration points\n* fin_el::FiniteElement   : Line(nod, nodof)\n\nRelated help\n\n?StructuralElement  : Help on structural elements\n?FiniteElement      : Help on finite element types\n?Line               : Help on a Line finite element\n\n\n\n"
},

{
    "location": "index.html#PtFEM.Beam",
    "page": "PtFEM.jl programmer's documentation",
    "title": "PtFEM.Beam",
    "category": "Type",
    "text": "Beam\n\nConcrete structural element with transverse and moment loading.\n\nConstructor\n\nBeam(ndim, nip, fin_el)\n\nArguments\n\n* ndim::Int64             : Number of dimensions\n* nst::Int64              : Number of stress terms\n* nxe::Int64              : Number of different property types\n* nip::Int64              : Number of integration points\n* direction::Symbol       : Number of integration points\n* fin_el::FiniteElement   : Line(nod, nodof)\n* axisymmetric::Bool      : Axisymmetric if true\n\nRelated help\n\n?StructuralElement  : Help on structural elements\n?FiniteElement      : Help on finite element types\n?Line               : Help on a Line finite element\n\n\n\n"
},

{
    "location": "index.html#Structural-Elements-1",
    "page": "PtFEM.jl programmer's documentation",
    "title": "Structural Elements",
    "category": "section",
    "text": "StructuralElement\nRod\nBeam"
},

{
    "location": "index.html#PtFEM.FiniteElement",
    "page": "PtFEM.jl programmer's documentation",
    "title": "PtFEM.FiniteElement",
    "category": "Type",
    "text": "FiniteElement\n\nAbstract finite element type.\n\nType\n\nabstract FiniteElement\n\nSubtypes\n\n* Line::FiniteElement          : 1D Line(nod, nodof)\n* Triangle::FiniteElement      : 2D Triangle(nod, nodof)\n* Quadrilateral::FiniteElement : 2D Quadrilateral(nod, nodof)\n* Hexahedron::FiniteElement    : 3D Hexahedron(nod, nodof)\n* Tetrahedron::FiniteElement   : 3D Tetrahedron(nod, nodof)\n\n\n\n"
},

{
    "location": "index.html#PtFEM.Line",
    "page": "PtFEM.jl programmer's documentation",
    "title": "PtFEM.Line",
    "category": "Type",
    "text": "Line (Interval)\n\n1D type finite element\n\nConstructor\n\nLine(nod, nodof)\nLine(nodof)\n\nArguments\n\n* nod::Int64       : Number of nodes for finite element, defaults to 2\n* nodof::Int64     : Number of degrees of freedom per node\n\nRelated help\n\n?FiniteElement      : Help on finite element types\n\n\n\n"
},

{
    "location": "index.html#PtFEM.Triangle",
    "page": "PtFEM.jl programmer's documentation",
    "title": "PtFEM.Triangle",
    "category": "Type",
    "text": "Triangle\n\n2D type finite element\n\nConstructor\n\nTriangle(nod, nodof)\n\nArguments\n\n* nod::Int64       : Number of nodes for finite element (3, 6, 10, 15)\n* nodof::Int64     : Number of degrees of freedom per node\n\nRelated help\n\n?FiniteElement      : Help on finite element types\n\n\n\n"
},

{
    "location": "index.html#Finite-Elements-1",
    "page": "PtFEM.jl programmer's documentation",
    "title": "Finite Elements",
    "category": "section",
    "text": "FiniteElement\nLine\nTriangle"
},

{
    "location": "index.html#PtFEM.fromSkyline-Tuple{Array{Float64,1},Array{Int64,1}}",
    "page": "PtFEM.jl programmer's documentation",
    "title": "PtFEM.fromSkyline",
    "category": "Method",
    "text": "fromSkyline\n\nHelper function to convert a Skyline vector to a full matrix.\n\nType\n\nfromSkyline(skyline::Vector{Float64}, kdiag::Vector{Int64})\n\nArguments\n\n* skyline::Vector{Float64}     : 1D Line(nod, nodof)\n* kdiag::Vector{Int64}         : 2D Triangle(nod, nodof)\n\n\n\n"
},

{
    "location": "index.html#PtFEM.sparin!-Tuple{Array{Float64,1},Array{Int64,1}}",
    "page": "PtFEM.jl programmer's documentation",
    "title": "PtFEM.sparin!",
    "category": "Method",
    "text": "Function sparin! performs Cholesky factorisation on a symmetric skyline global matrix. The vector kv is updated.\n\nArguments to sparin!(kv, kdiag):\n\nkv::Vector{Float64}   : Skyline vector of global stiffness matrix\n\nkdiag::Vector{Int64}  : Diagonal fin_el vector\n\n\n\n"
},

{
    "location": "index.html#PtFEM-1",
    "page": "PtFEM.jl programmer's documentation",
    "title": "PtFEM",
    "category": "section",
    "text": "fromSkyline(skyline::Vector{Float64}, kdiag::Vector{Int64})\nPtFEM.sparin!(kv::Vector{Float64}, kdiag::Vector{Int64})"
},

{
    "location": "index.html#Index-1",
    "page": "PtFEM.jl programmer's documentation",
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
