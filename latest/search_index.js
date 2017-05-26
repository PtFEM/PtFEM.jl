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
    "text": "This Julia package will contain the programs in \"Programming the Finite Element Method\" by I M Smith, D V Griffiths and L. Margetts (PtFEM). See TODO for the planned progress.PtFEM is a very versatile toolkit to construct FEM programs for practical engineering and scientific problems (to distinguish it somewhat from systems primarily focused on solving symbolic partial differential equations). Each chapter in the book gradually develops a set of related programs intended to be used as a starting point for a particular class of problems.I use PtFEM when referring to the book and PtFEM.jl when referring to the Julia package. "
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
    "location": "GETTINGSTARTED.html#Selected-examples-1",
    "page": "Getting started",
    "title": "Selected examples",
    "category": "section",
    "text": "Ex41.1.jl\nEx44.1.jl\nEx45.1.jl\nEx44.1.jl\nEx44.1.jl\nEx51.1.jl\nEx62.1.jl"
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
    "text": "The PtFEM book is the primary source to understand how the Fortran toolkit can be used to build FEM programs. This is also the case for the Julia version of the PtFEM toolkit, PtFEM.jl.But even with this restriction in place, there are many ways to port the PtFEM toolkit to Julia. Julia can in fact call the lower level Fortran \"building blocks\" (subroutines) directly. But that would make it harder to modify those functions. PtFEM.jl is entirely written in Julia end takes a middle of the road approach in replacing Fortran functionality by \"typical\" Julia features. These cases are documented in this file.If additional Julia versions of functions, particularly \"building blocks\", are required for use in the programs, these are added to the respective source files. Often times Julia's \"multiple dispatch\" takes care of selecting the correct version in the templates.Several Fortran routines will (over time) be deprecated, e.g. see below the discussion on the skyline storage format. For now the original Julia translations are kept in the src/deprecated directory.As more chapters will be added I will attempt to harmonize the input data dictionary"
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
    "text": "Method p41\n\nOne dimensional analysis of an axially loaded elastic Rod using 2-node  Line elements. \n\nConstructors\n\np41(data)\n\nArguments\n\n* `data::Dict{Symbol, Any}`  : Dictionary containing all input data\n\nRequired data dictionary keys\n\n* struc_el::StructuralElement                          : Type of  structural fin_el\n* support::Array{Tuple{Int,Array{Int,1}},1}        : Fixed-displacements vector\n* loaded_nodes::Array{Tuple{Int,Array{Float64,1}},1} : Node load vector\n* properties::Vector{Float64}                          : Material properties\n* x_coords::FloatRange{Float64}                        : x-coordinate vector\n\nOptional additional data dictionary keys\n\n* penalty = 1e20               : Penalty used for fixed degrees of freedoms\n* etype::Vector{Int}         : Element material vector if np_types > 1\n* eq_nodal_forces_and_moments  : Contribution of distributed loads to loaded_nodes\n\nReturn values\n\n* (jfem, dis_dt, fm_dt)        : Tuple of jFem, dis_dt and fm_dt\n                                 where:\n                                    jfem::jFem    : Computational result type\n                                    dis_dt        : Displacement data table\n                                    fm_dt         : Forces and moments data table\n\nRelated help\n\n?StructuralElement             : List of available structural element types\n?Rod                           : Help on a Rod structural element\n?FiniteElement                 : List finite element types\n?Line                          : Help on Line finite element\n\n\n\nMethod p41\n\nOne dimensional analysis of an axially loaded elastic Rod using 2-node  Line elements. \n\nConstructors\n\np41(m, data) # Re-use factored global stiffness matrix\n\nArguments\n\n* `m::jFEM`                  : Previously created jFEM model\n* `data::Dict{Symbol, Any}`  : Dictionary containing all input data\n\nRequired data dictionary keys\n\n* struc_el::StructuralElement                          : Type of  structural fin_el\n* support::Array{Tuple{Int,Array{Int,1}},1}        : Fixed-displacements vector\n* loaded_nodes::Array{Tuple{Int,Array{Float64,1}},1} : Node load vector\n* properties::Vector{Float64}                          : Material properties\n* x_coords::FloatRange{Float64}                        : x-coordinate vector\n\nOptional additional data dictionary keys\n\n* penalty = 1e20               : Penalty used for fixed degrees of freedoms\n* etype::Vector{Int}         : Element material vector if np_types > 1\n* eq_nodal_forces_and_moments  : Contribution of distributed loads to loaded_nodes\n\nReturn values\n\n* (jfem, dis_dt, fm_dt)        : Tuple of jFem, dis_dt and fm_dt\n                                 where:\n                                    jfem::jFem    : Computational result type\n                                    dis_dt        : Displacement data table\n                                    fm_dt         : Forces and moments data table\n\nRelated help\n\n?StructuralElement             : List of available structural element types\n?Rod                           : Help on a Rod structural element\n?FiniteElement                 : List finite element types\n?Line                          : Help on Line finite element\n\n\n\n"
},

{
    "location": "index.html#PtFEM.p42-Tuple{Dict{Symbol,Any}}",
    "page": "PtFEM.jl documentation",
    "title": "PtFEM.p42",
    "category": "Method",
    "text": "Method p42\n\nAnalysis of elastic pin-jointed frames using 2-node rod elements in 2- or 3-dimensions.\n\nConstructors\n\np42(data)\n\nArguments\n\n* `data::Dict{Symbol, Any}`  : Dictionary containing all input data\n\nDictionary keys\n\n* struc_el::StructuralElement                          : Type of structural element\n* support::Array{Tuple{Int,Array{Int,1}},1}        : Fixed-displacements vector\n* loaded_nodes::Array{Tuple{Int,Array{Float64,1}},1} : Node load vector\n* properties::Vector{Float64}                          : Material properties\n* x_coords::Vector{Float64}                            : x coordinate vector\n* y_coords::Vector{Float64}                            : y coordinate vector\n* g_num::Array{Int,2}                                : Element node connections\n\nOptional additional dictionary keys\n\n* penalty::Float64             : Penalty for fixed freedoms\n* etype::Vector{Int}         : Element material vector\n* z_coords::Vector{Float64}    : z coordinate vector (3D)\n* eq_nodal_forces_and_moments  : Contribution of distributed loads to loaded_nodes\n\nReturn values\n\n* (jfem, dis_dt, fm_dt)        : Tuple of jFem, dis_dt and fm_dt\n                                 where:\n                                    jfem::jFem    : Computational result type\n                                    dis_dt        : Displacement data table\n                                    fm_dt         : Forces and moments data table\n\nRelated help\n\n?StructuralElement  : List structural element types\n?Frame              : Help on a Rod structural fin_el\n?FiniteElement      : List finite element types\n?Line               : Help on Line finite element\n\n\n\n"
},

{
    "location": "index.html#PtFEM.p43-Tuple{Dict{Symbol,Any}}",
    "page": "PtFEM.jl documentation",
    "title": "PtFEM.p43",
    "category": "Method",
    "text": "p43\n\nAnalysis of elastic beams using 2-node Beam structural elements and Line finite elements. Elastic foundation is optional.\n\nConstructors\n\np43(data)\n\nArguments\n\n* `data::Dict{Symbol, Any}` : Dictionary containing all input data\n\nDictionary keys\n\n* struc_el::StructuralElement                          : Type of  structural fin_el\n* support::Array{Tuple{Int,Array{Int,1}},1}        : Fixed-displacements vector\n* loaded_nodes::Array{Tuple{Int,Array{Float64,1}},1} : Node load vector\n* properties::Vector{Float64}                          : Material properties\n* x_coords::LinSpace{Float64}                          : x coordinate vector\n* g_num::Array{Int,2}                                : Element node connections\n* fixed_freedoms::Array{Tuple{Vector{Int}}           : Fixed freedoms\n\nOptional additional dictionary keys\n\n* etype::Vector{Int}                                 : Element material vector\n* penalty::Float64                                     : Penalty for fixed freedoms\n* eq_nodal_forces_and_moments                          : Equivalent nodal loads\n\nReturn values\n\n* (jfem, dis_dt, fm_dt)        : Tuple of jFem, dis_dt and fm_dt\n                                 where:\n                                    jfem::jFem    : Computational result type\n                                    dis_dt        : Displacement data table\n                                    fm_dt         : Forces and moments data table\n\nRelated help\n\n?StructuralElement  : Help on structural elements\n?Rod                : Help on a Rod structural fin_el\n?FiniteElement      : Help on finite element types\n\n\n\n"
},

{
    "location": "index.html#PtFEM.p44-Tuple{Dict{Symbol,Any}}",
    "page": "PtFEM.jl documentation",
    "title": "PtFEM.p44",
    "category": "Method",
    "text": "p44\n\nAnalysis of elastic rigid-jointed frames using a 2-node Frame structural element and Line finite elements in 2 or 3 dimensions.\n\nConstructors\n\np44(data)\n\nArguments\n\n* `data::Dict{Symbol, Any}` : Dictionary containing all input data\n\nDictionary keys\n\n* struc_el::StructuralElement                          : Type of  structural fin_el\n* support::Array{Tuple{Int,Array{Int,1}},1}        : Fixed-displacements vector\n* loaded_nodes::Array{Tuple{Int,Array{Float64,1}},1} : Node load vector\n* properties::Vector{Float64}                          : Material properties\n* x_coords::FloatRange{Float64}                        : x coordinate vector\n* y_coords::FloatRange{Float64}                        : y coordinate vector\n* g_num::Array{Int,2}                                : Element node connections\n* fixed_freedoms::Array{Tuple{Vector{Int}}           : Fixed freedoms\n\nOptional additional dictionary keys\n\n* etype::Vector{Int}                                 : Element material vector\n* penalty::Float64                                     : Penalty for fixed freedoms\n* z_coords::FloatRange{Float64}                        : z coordinate vector\n* eq_nodal_forces_and_moments                          : Equivalent nodal loads\n\nReturn values\n\n* (jfem, dis_dt, fm_dt)        : Tuple of jFem, dis_dt and fm_dt\n                                 where:\n                                    jfem::jFem    : Computational result type\n                                    dis_dt        : Displacement data table\n                                    fm_dt         : Forces and moments data table\n\nRelated help\n\n?StructuralElement  : Help on structural elements\n?Beam               : Help on a Beam structural fin_el\n?FiniteElement      : Help on finite element types\n\n\n\n"
},

{
    "location": "index.html#PtFEM.p45-Tuple{Dict{Symbol,Any}}",
    "page": "PtFEM.jl documentation",
    "title": "PtFEM.p45",
    "category": "Method",
    "text": "Method p45\n\nAnalysis of elasto-plastic beams or rigid-jointed frames using a 2-node Frame structural element in 1, 2 or 3 dimensions. \n\nConstructors\n\np45(data)\n\nArguments\n\n* `data::Dict{Symbol, Any}`  : Dictionary containing all input data\n\nRequired data dictionary keys\n\n* struc_el::StructuralElement                          : Type of  structural element\n* support::Array{Tuple{Int,Array{Int,1}},1}        : Fixed-displacements vector\n* loaded_nodes::Array{Tuple{Int,Array{Float64,1}},1} : Node load vector\n* properties::Vector{Float64}                          : Material properties\n* x_coords::FloatRange{Float64}                        : x-coordinate vector\n* dload::FloatRange{Float64}                           : load steps\n\nOptional additional data dictionary keys\n\n* penalty = 1e20                 : Penalty used for fixed degrees of freedoms\n* etype::Vector{Int}           : Element material vector if np_types > 1\n* y_coords::FloatRange{Float64}  : y-coordinate vector (2D)\n* z_coords::FloatRange{Float64}  : x-coordinate vector (3D)\n* limit = 250                    : Iteration limit\n* tol = 0.0001                   : Tolerance for iteration convergence\n\nRelated help\n\n?StructuralElement             : List of available structural element types\n?Frame                         : Help on a Frame structural element\n?FiniteElement                 : List finite element types\n?Line                          : Help on Line finite element\n\n\n\n"
},

{
    "location": "index.html#PtFEM.p46-Tuple{Dict{Symbol,Any}}",
    "page": "PtFEM.jl documentation",
    "title": "PtFEM.p46",
    "category": "Method",
    "text": "Method p46\n\nStability (buckling) analysis of elastic beams using a 2-node Beam structural element and Line finite elements. Elastic foundation is optional.\n\nConstructors\n\np46(data)\n\nArguments\n\n* `data::Dict{Symbol, Any}` : Dictionary containing all input data\n\nRequired data dictionary keys\n\n* struc_el::StructuralElement                          : Type of  structural fin_el\n* support::Array{Tuple{Int,Array{Int,1}},1}        : Fixed-displacements vector\n* properties::Vector{Float64}                          : Material properties\n* x_coords::FloatRange{Float64}                        : x-coordinate vector\n\nOptional additional data dictionary keys\n\n* etype::Vector{Int}         : Element material vector if np_types > 1\n* limit = 250                  : Iteration limit\n* tol = 0.0001                 : Tolerance for iteration convergence\n\nRelated help\n\n?StructuralElement             : List of available structural element types\n?Beam                          : Help on a Beam structural element\n?FiniteElement                 : List finite element types\n?Line                          : Help on Line finite element\n\n\n\n"
},

{
    "location": "index.html#PtFEM.p47-Tuple{Dict{Symbol,Any}}",
    "page": "PtFEM.jl documentation",
    "title": "PtFEM.p47",
    "category": "Method",
    "text": "Method p47\n\nAnalysis of plates (Plane structural element) using 4-node Quadrilateral finite elements. Homogeneous material with identical elements. Mesh numbered in x or y direction.\n\nConstructors\n\np47(data)\n\nArguments\n\n* `data::Dict{Symbol, Any}` : Dictionary containing all input data\n\nRequired data dictionary keys\n\n* struc_el::StructuralElement                          : Structural element\n* support::Array{Tuple{Int,Array{Int,1}},1}        : Fixed-displacements vector\n* loaded_nodes::Array{Tuple{Int,Array{Float64,1}},1} : Node load vector\n* properties::Vector{Float64}                          : Material properties\n* x_coords::FloatRange{Floalt64}                       : x-coordinate vector\n* y_coords::FloatRange{Floalt64}                       : y-coordinate vector\n* thickness:: Float64                                  : Thickness of plate\n\nOptional additional data dictionary keys\n\n* penalty = 1e20               : Penalty used for fixed degrees of freedoms\n* etype::Vector{Int}         : Element material vector if np_types > 1\n\nReturn values\n\n* (fm_dt, sigma_dt)            : Tuple of jFem, dis_dt and fm_dt\n                                  where:\n                                    fm_dt         : Forces and moments data table\n                                    sigma_dt      : Stresses data table\n\nRelated help\n\n?StructuralElement             : List of available structural element types\n?Plane                         : Help on a Plane structural element\n?FiniteElement                 : List finite element types\n?Quadrilateral                 : Help on Quadrilateral finite element\n\n\n\n"
},

{
    "location": "index.html#Static-Equilibrium-Programs-1",
    "page": "PtFEM.jl documentation",
    "title": "4 Static Equilibrium Programs",
    "category": "section",
    "text": "p41\np42(data::Dict{Symbol, Any})\np43(data::Dict{Symbol, Any})\np44(data::Dict{Symbol, Any})\np45(data::Dict{Symbol, Any})\np46(data::Dict{Symbol, Any})\np47(data::Dict{Symbol, Any})"
},

{
    "location": "index.html#PtFEM.p51-Tuple{Dict{Symbol,Any}}",
    "page": "PtFEM.jl documentation",
    "title": "PtFEM.p51",
    "category": "Method",
    "text": "Method p51\n\nPlane or axisymmetric strain analysis of an elastic solid (Plane structural element) using 3-, 6-, 10- or 15-node right-angled triangles (Triangle finite elements) or 4-, 8- or 9-node rectangular quadrilaterals (Quadrilateral finite elements). Mesh numbered in x(r)- or y(z)- direction.\n\nConstructors\n\np51(data)\n\nArguments\n\n* `data::Dict{Symbol, Any}` : Dictionary containing all input data\n\nRequired data dictionary keys\n\n* struc_el::StructuralElement                          : Structural element\n* support::Array{Tuple{Int,Array{Int,1}},1}            : Fixed-displacements vector\n* loaded_nodes::Array{Tuple{Int,Array{Float64,1}},1}   : Node load vector\n* properties::Vector{Float64}                          : Material properties\n* x_coords::FloatRange{Floalt64}                       : x-coordinate vector\n* y_coords::FloatRange{Floalt64}                       : y-coordinate vector\n* thickness:: Float64                                  : Thickness of plate\n\nOptional additional data dictionary keys\n\n* penalty = 1e20             : Penalty used for fixed degrees of freedoms\n* etype::Vector{Int}         : Element material vector if np_types > 1\n\nReturn values\n\n* (fem, fm_dt, sigma_dt)     : Tuple of jFem, dis_dt and fm_dt\n                               where:\n                                 fm_dt         : Forces and moments data table\n                                 sigma_dt      : Stresses data table\n\nRelated help\n\n?StructuralElement           : List of available structural element types\n?Plane                       : Help on a Plane structural element\n?FiniteElement               : List finite element types\n?Quadrilateral               : Help on Quadrilateral finite element\n\n\n\n"
},

{
    "location": "index.html#Elastic-Solids-Programs-1",
    "page": "PtFEM.jl documentation",
    "title": "5 Elastic Solids Programs",
    "category": "section",
    "text": "p51(data::Dict{Symbol, Any})"
},

{
    "location": "index.html#PtFEM.p61-Tuple{Dict{Symbol,Any}}",
    "page": "PtFEM.jl documentation",
    "title": "PtFEM.p61",
    "category": "Method",
    "text": "Method p61\n\nPlane strain bearing capacity analysis of an elastic-plastic (von Mises) material using 8-node rectangular quadrilaterals. Viscoplastic strain method.\n\nConstructors\n\np61(data)\n\nArguments\n\n* `data::Dict{Symbol, Any}` : Dictionary containing all input data\n\nRequired data dictionary keys\n\n* struc_el::StructuralElement                          : Structural element\n* support::Array{Tuple{Int,Array{Int,1}},1}        : Fixed-displacements vector\n* loaded_nodes::Array{Tuple{Int,Array{Float64,1}},1} : Node load vector\n* properties::Vector{Float64}                          : Material properties\n* x_coords::FloatRange{Floalt64}                       : x-coordinate vector\n* y_coords::FloatRange{Floalt64}                       : y-coordinate vector\n* thickness:: Float64                                  : Thickness of plate\n* tol::Float64                                         : Convergence tolerance\n* qincs::Vector{Float64}                               : Incremental load steps\n\nOptional additional data dictionary keys\n\n* limit = 250                  : Iteration limit\n* penalty = 1e20               : Penalty used for fixed degrees of freedoms\n* etype::Vector{Int}         : Element material vector if np_types > 1\n\nReturn values\n\n* (g_coord, g_num, disp)        : where:\n                                    g_coord  : Coordinates\n                                    g_num    : Node numbering\n                                    disp     : Matrix of displacements\n\nRelated help\n\n?StructuralElement             : List of available structural element types\n?Plane                         : Help on a Plane structural element\n?FiniteElement                 : List finite element types\n?Quadrilateral                 : Help on Quadrilateral finite element\n\n\n\n"
},

{
    "location": "index.html#PtFEM.p62-Tuple{Dict{Symbol,Any}}",
    "page": "PtFEM.jl documentation",
    "title": "PtFEM.p62",
    "category": "Method",
    "text": "Method p62\n\nPlane strain bearing capacity analysis of an elastic-plastic (von Mises) material using 8-node rectangular quadrilaterals.\n\nViscoplastic strain method.\n\nNo global stiffness matrix assembly.\n\nDiagonally preconditioned conjugate gradient solver.\n\nConstructors\n\np62(data)\n\nArguments\n\n* `data::Dict{Symbol, Any}` : Dictionary containing all input data\n\nRequired data dictionary keys\n\n* struc_el::StructuralElement                          : Structural element\n* support::Array{Tuple{Int,Array{Int,1}},1}        : Fixed-displacements vector\n* loaded_nodes::Array{Tuple{Int,Array{Float64,1}},1} : Node load vector\n* properties::Vector{Float64}                          : Material properties\n* x_coords::FloatRange{Floalt64}                       : x-coordinate vector\n* y_coords::FloatRange{Floalt64}                       : y-coordinate vector\n* thickness:: Float64                                  : Thickness of plate\n* tol::Float64                                         : Convergence tolerance\n* qincs::Vector{Float64}                               : Incremental load steps\n\nOptional additional data dictionary keys\n\n* limit = 250                  : Iteration limit\n* penalty = 1e20               : Penalty used for fixed degrees of freedoms\n* etype::Vector{Int}         : Element material vector if np_types > 1\n\nReturn values\n\n* (g_coord, g_num, disp)        : where:\n                                    g_coord  : Coordinates\n                                    g_num    : Node numbering\n                                    disp     : Matrix of displacements\n\nRelated help\n\n?StructuralElement             : List of available structural element types\n?Plane                         : Help on a Plane structural element\n?FiniteElement                 : List finite element types\n?Quadrilateral                 : Help on Quadrilateral finite element\n\n\n\n"
},

{
    "location": "index.html#Material-Nonlinearity-Programs-1",
    "page": "PtFEM.jl documentation",
    "title": "6 Material Nonlinearity Programs",
    "category": "section",
    "text": "p61(data::Dict{Symbol, Any})\np62(data::Dict{Symbol, Any})"
},

{
    "location": "index.html#PtFEM.StructuralElement",
    "page": "PtFEM.jl documentation",
    "title": "PtFEM.StructuralElement",
    "category": "Type",
    "text": "StructuralElement\n\nAbstract structural element type.\n\nType\n\nabstract StructuralElement\n\nSubtypes\n\n* Rod::StructuralElement          : Rod(nxe, np_types, nip, fin_el)\n* Beam::StructuralElement         : Beam(nod, nodof)\n* Frame::StructuralElement        : Frame(nod, nodof)\n* Plane::StructuralElement        : Plane(nod, nodof)\n* Solid::StructuralElement        : Solid(nod, nodof)\n* GenericSolid::StructuralElement : GenericSolid(nod, nodof)\n\nRelated help\n\n?FiniteElement                   : Show all finite elements\n?Rod                              : Help on Rod structural element\n?Beam                             : Help on Beam structural element\n?Frame                            : Help on Frame structural element\n?Plane                            : Help on Plane structural element\n?Solid                            : Help on Solid structural element\n?GenericSolid                     : Help on GenericSolid structural element\n\n\n\n"
},

{
    "location": "index.html#PtFEM.Rod",
    "page": "PtFEM.jl documentation",
    "title": "PtFEM.Rod",
    "category": "Type",
    "text": "Rod\n\nConcrete 1D structural element with only axial stresses.\n\nConstructor\n\nRod(nels, np_types, nip, fin_el)\n\nArguments\n\n* nels::Int             : Number of fin_els (stored in field nxe)\n* np_types::Int         : Number of different property types\n* nip::Int              : Number of integration points\n* fin_el::FiniteElement   : Line(nod, nodof)\n\nRelated help\n\n?StructuralElement  : Help on structural elements\n?FiniteElement      : Help on finite element types\n?Line               : Help on a Line finite element\n\n\n\n"
},

{
    "location": "index.html#PtFEM.Beam",
    "page": "PtFEM.jl documentation",
    "title": "PtFEM.Beam",
    "category": "Type",
    "text": "Beam\n\nConcrete structural element with transverse and moment loading.\n\nConstructor\n\nBeam(ndim, nip, fin_el)\n\nArguments\n\n* ndim::Int             : Number of dimensions\n* nst::Int              : Number of stress terms\n* nxe::Int              : Number of different property types\n* nip::Int              : Number of integration points\n* direction::Symbol       : Number of integration points\n* fin_el::FiniteElement   : Line(nod, nodof)\n* axisymmetric::Bool      : Axisymmetric if true\n\nRelated help\n\n?StructuralElement  : Help on structural elements\n?FiniteElement      : Help on finite element types\n?Line               : Help on a Line finite element\n\n\n\n"
},

{
    "location": "index.html#PtFEM.Frame",
    "page": "PtFEM.jl documentation",
    "title": "PtFEM.Frame",
    "category": "Type",
    "text": "Frame\n\nPin- or rigid-jointed structural element.\n\nConstructor\n\nFrame(nels, nn, ndim, finite_element(nod, nodof))\n\nArguments\n\n* nels::Int             : Number of elements\n* nn:Int                : Number of nodes\n* ndim::Int             : Number of dimensions\n* nst::Int              : Number of stress terms\n* nip::Int              : Number of integration points\n* fin_el::FiniteElement   : Line(nod, nodof)\n\nRelated help\n\n?StructuralElement  : List structural elements\n?FiniteElement      : List finite element types\n?Line               : Help on a Line finite element\n\n\n\n"
},

{
    "location": "index.html#PtFEM.Plane",
    "page": "PtFEM.jl documentation",
    "title": "PtFEM.Plane",
    "category": "Type",
    "text": "Plane\n\nPlate structural element.\n\nConstructor\n\nPlane(ndim, nst, nxe, nye, nip, dir, finite_element(nod, nodof), axisymmetric)\n\nArguments\n\n* ndim::Int             : Number of dimensions\n* nst::Int              : Number of stress terms\n* nxe::Int              : Number of elements in x direction\n* nye::Int              : Number of elements in y direction\n* nip::Int              : Number of integration points\n* dir::Symbol             : Direction of node numbering\n* fin_el::FiniteElement   : Line(nod, nodof)\n* axisymmetric::Bool      : Axisymmetric\n\nRelated help\n\n?StructuralElement  : List structural elements\n?FiniteElement      : List finite element types\n?Line               : Help on a Line finite element\n\n\n\n"
},

{
    "location": "index.html#PtFEM.Solid",
    "page": "PtFEM.jl documentation",
    "title": "PtFEM.Solid",
    "category": "Type",
    "text": "Solid\n\nSolid structural element.\n\nConstructor\n\nSolid(ndim, nst, nxe, nye, nze, nip, finite_element(nod, nodof))\n\nArguments\n\n* ndim::Int             : Number of dimensions\n* nst::Int              : Number of stress terms\n* nxe::Int              : Number of elements in x direction\n* nye::Int              : Number of elements in y direction\n* nze::Int              : Number of elements in z direction\n* nip::Int              : Number of integration points\n* fin_el::FiniteElement   : Line(nod, nodof)\n\nRelated help\n\n?StructuralElement  : List structural elements\n?FiniteElement      : List finite element types\n\n\n\n"
},

{
    "location": "index.html#PtFEM.GenericSolid",
    "page": "PtFEM.jl documentation",
    "title": "PtFEM.GenericSolid",
    "category": "Type",
    "text": "GenericSolid\n\nSolid structural element.\n\nConstructor\n\nGenericSolid(ndim, nst, nels, nn, nip, finite_element(nod, nodof), axisymmetric)\n\nArguments\n\n* ndim::Int             : Number of dimensions\n* nst::Int              : Number of stress terms\n* nels::Int             : Number of finite elements\n* nn::Int               : Number of nodes\n* nip::Int              : Number of integration points\n* fin_el::FiniteElement   : Finite element type used\n* axisymmetric::Bool      : Axisymmetric\n\nRelated help\n\n?StructuralElement  : List structural elements\n?FiniteElement      : List finite element types\n\n\n\n"
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
    "text": "Line (Interval)\n\n1D type finite element\n\nConstructor\n\nLine(nod, nodof)\nLine(nodof)\n\nArguments\n\n* nod::Int       : Number of nodes for finite element, defaults to 2\n* nodof::Int     : Number of degrees of freedom per node\n\nRelated help\n\n?FiniteElement      : Help on finite element types\n\n\n\n"
},

{
    "location": "index.html#PtFEM.Triangle",
    "page": "PtFEM.jl documentation",
    "title": "PtFEM.Triangle",
    "category": "Type",
    "text": "Triangle\n\n2D type finite element\n\nConstructor\n\nTriangle(nod, nodof)\n\nArguments\n\n* nod::Int       : Number of nodes for finite element (3, 6, 10, 15)\n* nodof::Int     : Number of degrees of freedom per node\n\nRelated help\n\n?FiniteElement      : Help on finite element types\n\n\n\n"
},

{
    "location": "index.html#PtFEM.Quadrilateral",
    "page": "PtFEM.jl documentation",
    "title": "PtFEM.Quadrilateral",
    "category": "Type",
    "text": "Quadrilateral\n\n2D type finite element\n\nConstructor\n\nQuadrilateral(nod, nodof)\n\nArguments\n\n* nod::Int       : Number of nodes for finite element (4, 8, 9)\n* nodof::Int     : Number of degrees of freedom per node\n\nRelated help\n\n?FiniteElement      : Help on finite element types\n\n\n\n"
},

{
    "location": "index.html#PtFEM.Hexahedron",
    "page": "PtFEM.jl documentation",
    "title": "PtFEM.Hexahedron",
    "category": "Type",
    "text": "hexahedron\n\n3D type finite element\n\nConstructor\n\nHexahedron(nod, nodof)\n\nArguments\n\n* nod::Int       : Number of nodes for finite element (8, 14, 20)\n* nodof::Int     : Number of degrees of freedom per node\n\nRelated help\n\n?FiniteElement      : Help on finite element types\n\n\n\n"
},

{
    "location": "index.html#PtFEM.Tetrahedron",
    "page": "PtFEM.jl documentation",
    "title": "PtFEM.Tetrahedron",
    "category": "Type",
    "text": "Tetrahedron\n\n3D type finite element\n\nConstructor\n\nTetrahedron(nod, nodof)\nTetrahedron(nodof)\n\nArguments\n\n* nod::Int       : Number of nodes for finite element (defaults to 4)\n* nodof::Int     : Number of degrees of freedom per node\n\nRelated help\n\n?FiniteElement      : Help on finite element types\n\n\n\n"
},

{
    "location": "index.html#Finite-Element-Types-1",
    "page": "PtFEM.jl documentation",
    "title": "Finite Element Types",
    "category": "section",
    "text": "FiniteElement\nLine\nTriangle\nQuadrilateral\nHexahedron\nTetrahedron"
},

{
    "location": "index.html#PtFEM.FEM",
    "page": "PtFEM.jl documentation",
    "title": "PtFEM.FEM",
    "category": "Type",
    "text": "FEM\n\nComputational structure used in chapter 5 (Skyline format used)\n\n\n\n"
},

{
    "location": "index.html#PtFEM.jFEM",
    "page": "PtFEM.jl documentation",
    "title": "PtFEM.jFEM",
    "category": "Type",
    "text": "jFEM\n\nComputational structure used in chapter 4 (Julia Sparse matrices used)\n\n\n\n"
},

{
    "location": "index.html#Other-Julia-Types-1",
    "page": "PtFEM.jl documentation",
    "title": "Other Julia Types",
    "category": "section",
    "text": "FEM\njFEM"
},

{
    "location": "index.html#PtFEM.beam_gm!-Tuple{Array{Float64,2},Float64}",
    "page": "PtFEM.jl documentation",
    "title": "PtFEM.beam_gm!",
    "category": "Method",
    "text": "beam_gm!\n\nThis subroutine forms the beam geometric matrix for stability analysis.\n\nMethod\n\nbeam_gm!(gm::Matrix{Float64}, ell::Float64)\n\nArguments\n\n* gm::::Matrix{Float64}     : Geometric matrix for beam element (Updated)\n* ell::Float64              : Element length\n\n\n\n"
},

{
    "location": "index.html#PtFEM.beam_km!-Tuple{Array{Float64,2},Float64,Float64}",
    "page": "PtFEM.jl documentation",
    "title": "PtFEM.beam_km!",
    "category": "Method",
    "text": "beam_km!\n\nThis subroutine forms the stiffness matrix of a beam element (bending only).\n\nMethod\n\nbeam_km!(km, ei, ell)\n\nArguments\n\n* km::::Matrix{Float64}     : Stiiness matrix for beam element (Updated)\n* ei::Float64               : Element stiffness\n* ell::Float64              : Element length\n\n\n\n"
},

{
    "location": "index.html#PtFEM.beam_mm!-Tuple{Array{Float64,2},Float64,Float64}",
    "page": "PtFEM.jl documentation",
    "title": "PtFEM.beam_mm!",
    "category": "Method",
    "text": "beam_mm!\n\nThis subroutine forms the stiffness matrix of a beam element (bending only).\n\nMethod\n\nbeam_mm!(mm, ei, ell)\n\nArguments\n\n* mm::::Matrix{Float64}     : Mass matrix for beam element (Updated)\n* fs::Float64               : Element density\n* ell::Float64              : Element length\n\n\n\n"
},

{
    "location": "index.html#PtFEM.beemat!-Tuple{Array{Float64,2},Array{Float64,2}}",
    "page": "PtFEM.jl documentation",
    "title": "PtFEM.beemat!",
    "category": "Method",
    "text": "beemat!\n\nThis subroutine forms the strain-displacement matrix for axisymmetric solids subjected to non-axisymmetric loading.\n\nMethod\n\nbeemat!(bee, deriv)\n\nArguments\n\n* bee::Matrix{Float64}         : Bee matrix (Updated)\n* deriv::Matrix{Float64}       : Derivative\n\n\n\n"
},

{
    "location": "index.html#PtFEM.bmat_nonaxi!-Tuple{Array{Float64,2},Float64,Array{Float64,2},Array{Float64,2},Array{Float64,1},Int64,Int64}",
    "page": "PtFEM.jl documentation",
    "title": "PtFEM.bmat_nonaxi!",
    "category": "Method",
    "text": "bmat_nonaxi!\n\nThis subroutine forms the strain-displacement matrix for axisymmetric solids subjected to non-axisymmetric loading.\n\nMethod\n\nbmat_nonaxi!(bee, radius, coord, deriv, fun, iflag, lth)\n\nArguments\n\n* bee::Matrix{Float64}         : Bee matrix (Updated)\n* radius::Float64              : r coordinate of the Gauss point\n* coord::Matrix{Float64}       : Nodal coordinate matrix\n* deriv::Matrix{Float64}       : Derivative\n* fun::Vector{Float64}         : Shape function\n* iflag::Int                 : 1 = symmetric, -1 = anti-symmetric\n* lth::Int                   : Harmonic\n\n\n\n"
},

{
    "location": "index.html#PtFEM.checon!-Tuple{Array{Float64,1},Array{Float64,1},Float64}",
    "page": "PtFEM.jl documentation",
    "title": "PtFEM.checon!",
    "category": "Method",
    "text": "checon!\n\nThis subroutine sets converged to .FALSE. if relative change in loads and oldlds is greater than tol and updates oldlds.\n\nMethod\n\nchecon!(loads, oldlds, tol)\n\nArguments\n\n* loads::Vector{Float64}        : Displacements vector\n* oldlds::Vector{Float64}       : Previous displacement vector\n* tol::Float64                  : Convergence tolerance\n\n\n\n"
},

{
    "location": "index.html#PtFEM.deemat!-Tuple{Array{Float64,2},Float64,Float64}",
    "page": "PtFEM.jl documentation",
    "title": "PtFEM.deemat!",
    "category": "Method",
    "text": "deemat!\n\nThis subroutine returns the elastic dee matrix for ih=3 (plane strain), ih=4 (axisymmetry or plane strain elastoplasticity) or ih=6 (three dimensions).\n\nMethod\n\ndeemat!(dee, e, v)\n\nArguments\n\n* dee::Matrix{Float64}         : Dee matrix (Updated)\n* e::Float64                   : Young's modulus\n* v::Float64                   : Poisson's ratio\n\n\n\n"
},

{
    "location": "index.html#PtFEM.fkdiag!-Tuple{Array{Int64,1},Array{Int64,1}}",
    "page": "PtFEM.jl documentation",
    "title": "PtFEM.fkdiag!",
    "category": "Method",
    "text": "fkdiag!\n\nThis subroutine returns the elastic dee matrix for ih=3 (plane strain), ih=4 (axisymmetry or plane strain elastoplasticity) or ih=6 (three dimensions).\n\nMethod\n\nfkdiag!(kdiag, g)\n\nArguments\n\n* kdiag::Vector{Int}      : Bandwidth vector (Updated)\n* g::Vector{Int}          : Element steering vector\n\n\n\n"
},

{
    "location": "index.html#PtFEM.fmplat!-Tuple{Array{Float64,1},Array{Float64,1},Array{Float64,1},Array{Float64,2},Float64,Float64,Int64}",
    "page": "PtFEM.jl documentation",
    "title": "PtFEM.fmplat!",
    "category": "Method",
    "text": "fmplat!\n\nThis subroutine forms the 2nd derivatives for rectangular plate bending fin_els.\n\nMethod\n\nfmplat!(d2x, d2y, d2xy, points, aa, bb, i)\n\nArguments\n\n* d2x::Vector{Float64}       : x derivative term (Updated)\n* d2y::Vector{Float64}       : y derivative term (Updated)\n* d2xy::Vector{Float64}      : x,y derivative term (Updated)\n* points::Matrix{Float64}    : Location of Gauss points\n* aa::Float64                : Dimension of plate\n* bb::Float64                : Dimension of plate\n* i::Int                   : Gauss point to use\n\n\n\n"
},

{
    "location": "index.html#PtFEM.formm!-Tuple{Array{Float64,1},Array{Float64,2},Array{Float64,2},Array{Float64,2}}",
    "page": "PtFEM.jl documentation",
    "title": "PtFEM.formm!",
    "category": "Method",
    "text": "formm!\n\nThis subroutine forms the derivatives of the invariants with respect to stress in 2- or 3-d. See equation 6.25.\n\nFunction\n\nformm!(stress, m1, m2, m3)\n\nArguments\n\n* stress::Vector{Float64}    : Stress vector, see eq 6.25\n* m1::Matrix{Float64}        : m1 matrix\n* m2::Matrix{Float64}        : m2 matrix\n* m3::Matrix{Float64}        : m3 matrix\n\n\n\n"
},

{
    "location": "index.html#PtFEM.formnf!-Tuple{Int64,Int64,Array{Int64,2}}",
    "page": "PtFEM.jl documentation",
    "title": "PtFEM.formnf!",
    "category": "Method",
    "text": "formnf!\n\nReturns nodal freedom numbering array nf\n\nFunction\n\nformnf!(nodof, nn, nf)\n\nArguments\n\n* nodof::Int       : Number of degrees of freedom for each node\n* nn::Int          : Number of nodes in mesh\n* nf::Array{Int,2} : Nodal freedom matrix (updated)\n\n\n\n"
},

{
    "location": "index.html#PtFEM.fsparm!-Tuple{Any,Any,Any}",
    "page": "PtFEM.jl documentation",
    "title": "PtFEM.fsparm!",
    "category": "Method",
    "text": "fsparm!\n\nFunction fsparm assembles fin_el matrices into a Julia sparse global stiffness matrix.\n\nMethod\n\nfsparm!(gsm, g, km)\n\nArguments\n\n* gsm::SparseArrays{Float64, Float64}   : Sparse stiffnes matrix (Updated)\n* g::Vector{Int}                      : Global coordinate vector.\n* km::Matrix{Float64}                   : Stiffness matrix.\n\n\n\n"
},

{
    "location": "index.html#PtFEM.glob_to_loc!-Tuple{Array{Float64,1},Array{Float64,1},Float64,Array{Float64,2}}",
    "page": "PtFEM.jl documentation",
    "title": "PtFEM.glob_to_loc!",
    "category": "Method",
    "text": "glob_to_loc!\n\nThis subroutine transforms the global end reactions and moments into the local system (2- or 3-d). Called from hinge!().\n\nFunction\n\nglob_to_loc!(loc, glob, gamma, coord)\n\nArguments\n\n* loc::Vector{Float64}       : Local force and momemts (Updated)\n* glob::Vector{Float64}      : Globale forces and moments\n* gamma::Float64             : Element orientation angle (3D)\n* coord::Matrix{Float64}     : Nodal coordinates\n\n\n\n"
},

{
    "location": "index.html#PtFEM.glob_to_axial-Tuple{Array{Float64,1},Array{Float64,2}}",
    "page": "PtFEM.jl documentation",
    "title": "PtFEM.glob_to_axial",
    "category": "Method",
    "text": "glob_to_axial!\n\nThis subroutine transforms the global end reactions into an axial force for rod fin_els (2- or 3-d).\n\nFunction\n\nglob_to_axial!(glob, coord)\n\nArguments\n\n* glob::Vector{Float64}      : Globale forces and moments\n* coord::Matrix{Float64}     : Nodal coordinates\n\n\n\n"
},

{
    "location": "index.html#PtFEM.hinge!-Tuple{Array{Float64,2},Array{Float64,2},Array{Float64,1},Array{Float64,1},Array{Float64,2},Any,Array{Int64,1},Array{Float64,1}}",
    "page": "PtFEM.jl documentation",
    "title": "PtFEM.hinge!",
    "category": "Method",
    "text": "hinge!\n\nThis subroutine forms the end forces and moments to be applied to a member if a joint has gone plastic.\n\nFunction\n\nhinge!(coord, holdr, action, react, prop, iel, etype, gamma)\n\nArguments\n\n* coord::Matrix{Float64}     : Nodal coordinates\n* holdr::Matrix{Float64}     : Existing reactions\n* action::Vector{Float64}    : Incremental reactions\n* react::Vector{Float64}     : Correction to reactions (Updated)\n* prop::Matrix{Float64}      : Beam properties\n* iel::Int                 : Element number\n* etype::Vector{Int}       : Element type\n* gamma::Vector{Float64}     : Element orientation (3D)\n\n\n\n"
},

{
    "location": "index.html#PtFEM.invar!-Tuple{Array{Float64,1},Float64,Float64,Float64}",
    "page": "PtFEM.jl documentation",
    "title": "PtFEM.invar!",
    "category": "Method",
    "text": "invar!\n\nThis subroutine forms the stress invariants in 2- or 3-d. See equations 6.3 and 6.4\n\nFunction\n\ninvar!(stress, sigm, dsbar, theta)\n\nArguments\n\n* stress::Vector{Float64}    : Stress vector\n* sigm::Float64              : Invariant, eq 6.4 (Updated)\n* dsbar::Float64             : Invariant, eq 6.4 (Updated)\n* theta::Float64             : Invariant, eq 6.3 (Updated)\n\n\n\n"
},

{
    "location": "index.html#PtFEM.linmul_sky!-Tuple{Array{Float64,1},Array{Float64,1},Array{Float64,1},Array{Int64,1}}",
    "page": "PtFEM.jl documentation",
    "title": "PtFEM.linmul_sky!",
    "category": "Method",
    "text": "linmul_sky!\n\nThis subroutine forms the product of symmetric matrix stored as a skyline and a vector.\n\nMethod\n\nlinmul_sky!(kv, disps, loads, kdiag)\n\nArguments\n\n* kv::Vector{Float64}       : Sparse stiffnes matrix (Skyline format)\n* disps::Vector{Float64}    : Displacements\n* loads::Vector{Float64}    : Loads vector (Updated)\n* kdiag::Vector{Int}      : Bandwidth vector\n\n\n\n"
},

{
    "location": "index.html#PtFEM.loc_to_glob!-Tuple{Array{Float64,1},Array{Float64,1},Float64,Array{Float64,2}}",
    "page": "PtFEM.jl documentation",
    "title": "PtFEM.loc_to_glob!",
    "category": "Method",
    "text": "loc_to_glob!\n\nThis subroutine transforms the local end reactions and moments into the global system (3-d).\n\nFunction\n\nloc_to_glob!(loc, glob, gamma, coord)\n\nArguments\n\n* loc::Vector{Float64}       : Local force and momemts (Updated)\n* glob::Vector{Float64}      : Globale forces and moments\n* gamma::Float64             : Element orientation angle (3D)\n* coord::Matrix{Float64}     : Nodal coordinates\n\n\n\n"
},

{
    "location": "index.html#PtFEM.num_to_g!-Tuple{Array{Int64,1},Array{Int64,2},Array{Int64,1}}",
    "page": "PtFEM.jl documentation",
    "title": "PtFEM.num_to_g!",
    "category": "Method",
    "text": "num_to_g!\n\nReturns the element steering vector g from the element node numbering num and the nodal freedom array nf.\n\nFunction\n\nnum_to_g!(num, nf, g)\n\nArguments\n\n* num::Vector{Int}       : Node numbering vector\n* nf::Matrix{Int}        : Nodal freedom array\n* g::Vector{Int}         : Element steering vector (Updated)\n\n\n\n"
},

{
    "location": "index.html#PtFEM.pin_jointed!-Tuple{Array{Float64,2},Float64,Array{Float64,2}}",
    "page": "PtFEM.jl documentation",
    "title": "PtFEM.pin_jointed!",
    "category": "Method",
    "text": "pin_jointed!\n\nThis subroutine forms the global stiffness matrix of a general pin-joionted structural element (1-, 2- or 3-d).\n\nFunction\n\npin_jointed!(km, ea, coord)\n\nArguments\n\n* km::Matrix{Float64}       : Element stiffness matrix (Updated)\n* ea::Float64               : Element stiffness\n* coord::Matrix{Float64}}   : Element nodal coordinates\n\n\n\n"
},

{
    "location": "index.html#PtFEM.rigid_jointed!-Tuple{Array{Float64,2},Array{Float64,2},Array{Float64,1},Array{Int64,1},Int64,Array{Float64,2}}",
    "page": "PtFEM.jl documentation",
    "title": "PtFEM.rigid_jointed!",
    "category": "Method",
    "text": "rigid_jointed!\n\nThis subroutine forms the global stiffness matrix of a general pin-joionted structural element (1-, 2- or 3-d).\n\nFunction\n\nrigid_jointed!(km, prop, gamma, etype, iel, coord)\n\nArguments\n\n* km::Matrix{Float64}       : Element stiffness matrix (Updated)\n* prop::Matrix{Float64}     : Element properties\n* gamma::Vector{Float64}    : Element orientations (3D)\n* etype::Vector{Int}      : Element type vector\n* iel::Int                : Element number\n* coord::Matrix{Float64}}   : Element nodal coordinates\n\n\n\n"
},

{
    "location": "index.html#PtFEM.rod_km!-Tuple{Array{Float64,2},Float64,Float64}",
    "page": "PtFEM.jl documentation",
    "title": "PtFEM.rod_km!",
    "category": "Method",
    "text": "rod_km!\n\nThis subroutine forms the stiffness matrix of a 1-d \"rod\" fin_el.\n\nFunction\n\nrod_km!(km, ea, length)\n\nArguments\n\n* km::Matrix{Float64}       : Element stiffness matrix (Updated)\n* ea::Float64               : Element stiffness\n* ell::Float64              : Element length\n\n\n\n"
},

{
    "location": "index.html#PtFEM.rod_mm!-Tuple{Array{Float64,2},Float64}",
    "page": "PtFEM.jl documentation",
    "title": "PtFEM.rod_mm!",
    "category": "Method",
    "text": "rod_mm!\n\nThis subroutine forms the consistent mass matrix of a 1-d \"rod\" fin_el.\n\nFunction\n\nrod_mm!(km, ell)\n\nArguments\n\n* mm::Matrix{Float64}       : Element mass matrix (Updated)\n* ell::Float64              : Element length\n\n\n\n"
},

{
    "location": "index.html#PtFEM.sample!",
    "page": "PtFEM.jl documentation",
    "title": "PtFEM.sample!",
    "category": "Function",
    "text": "sample!\n\nThis subroutine returns the local coordinates and weighting coefficients of the integrating points.\n\nFunction\n\nsample!(fin_el, s, wt)\n\nArguments\n\n* fin_el::FiniteElement      : Finite element type\n* s::Matrix{Float64}        : Local coordinates (Updated)\n* wt::Vector{Float64}       : Weighting coefficients (Updated)\n\n\n\n"
},

{
    "location": "index.html#PtFEM.shape_der!-Tuple{Array{Float64,2},Array{Float64,2},Int64}",
    "page": "PtFEM.jl documentation",
    "title": "PtFEM.shape_der!",
    "category": "Method",
    "text": "shape_der!\n\nThis subroutine produces derivatives of shape functions with respect to local coordinates.\n\nFunction\n\nshape_der!(der, point, i)\n\nArguments\n\n* der::Matrix{Float64}       : Function derivative (Updated)\n* points::Matrix{Float64}    : Local coordinates of integration points\n* i::Int                   : Integration point\n\n\n\n"
},

{
    "location": "index.html#PtFEM.shape_fun!-Tuple{Array{Float64,1},Array{Float64,2},Int64}",
    "page": "PtFEM.jl documentation",
    "title": "PtFEM.shape_fun!",
    "category": "Method",
    "text": "shape_fun!\n\nThis subroutine produces derivatives of shape functions with respect to local coordinates.\n\nFunction\n\nshape_fun!(fun, point, i)\n\nArguments\n\n* fun::Vector{Float64}       : Shape function (Updated)\n* points::Matrix{Float64}    : Local coordinates of integration points\n* i::Int                   : Integration point\n\n\n\n"
},

{
    "location": "index.html#PtFEM.stability-Tuple{SparseMatrixCSC{Float64,Int64},SparseMatrixCSC{Float64,Int64},Float64,Int64}",
    "page": "PtFEM.jl documentation",
    "title": "PtFEM.stability",
    "category": "Method",
    "text": "stability\n\nFunction spabac! performs Cholesky forward and back-substitution on a symmetric skyline global matrix. The loads vector is updated.\n\n###Arguments\n\nstability(gsm, ggm, tol, limit)\n\nArguments\n\n* gsm::SparseMatrixCSC{Float64,Int}   : Factored global stiffness matrix\n* ggm::SparseMatrixCSC{Float64,Int}   : Factored geometric matrix\n* tol::Float64                          : Convergence tolerance\n* limit::Int                          : Iteration limit\n\n\n\n"
},

{
    "location": "index.html#PtFEM-Main-1",
    "page": "PtFEM.jl documentation",
    "title": "PtFEM - Main",
    "category": "section",
    "text": "PtFEM.beam_gm!(gm::Matrix{Float64}, ell::Float64)\nPtFEM.beam_km!(km::Matrix{Float64}, ei::Float64, ell::Float64)\nPtFEM.beam_mm!(mm::Matrix{Float64}, fs::Float64, ell::Float64)\nPtFEM.beemat!(bee::Matrix{Float64},deriv::Matrix{Float64})\nPtFEM.bmat_nonaxi!(bee::Matrix{Float64}, radius::Float64, coord::Matrix{Float64}, deriv::Matrix{Float64}, fun::Vector{Float64}, iflag::Int, lth::Int)\nPtFEM.checon!(loads::Vector{Float64}, oldlds::Vector{Float64}, tol::Float64)\nPtFEM.deemat!(dee::Array{Float64, 2}, e::Float64, v::Float64)\nPtFEM.fkdiag!(kdiag::Vector{Int}, g::Vector{Int})\nPtFEM.fmplat!(d2x::Vector{Float64}, d2y::Vector{Float64}, d2xy::Vector{Float64}, points::Matrix{Float64}, aa::Float64, bb::Float64, i::Int)\nPtFEM.formm!(stress::Vector{Float64}, m1::Matrix{Float64}, m2::Matrix{Float64}, m3::Matrix{Float64})\nPtFEM.formnf!(nodof::Int, nn::Int, nf::Matrix{Int})\nPtFEM.fsparm!(gsm, g, km)\nPtFEM.glob_to_loc!(loc::Vector{Float64}, glob::Vector{Float64}, gamma::Float64, coord::Matrix{Float64})\nPtFEM.glob_to_axial(glob::Vector{Float64}, coord::Matrix{Float64})\nPtFEM.hinge!(coord::Matrix{Float64}, holdr::Matrix{Float64}, action::Vector{Float64}, react::Vector{Float64}, prop::Matrix{Float64}, iel, etype::Vector{Int}, gamma::Vector{Float64})\nPtFEM.invar!(stress::Vector{Float64}, sigm::Float64, dsbar::Float64, theta::Float64)\nPtFEM.linmul_sky!(kv::Vector{Float64}, disps::Vector{Float64}, loads::Vector{Float64}, kdiag::Vector{Int})\nPtFEM.loc_to_glob!(loc::Vector{Float64}, glob::Vector{Float64}, gamma::Float64, coord::Matrix{Float64})\nPtFEM.num_to_g!(num::Vector{Int}, nf::Matrix{Int}, g::Vector{Int})\nPtFEM.pin_jointed!(km::Matrix{Float64}, ea::Float64, coord::Matrix{Float64})\nPtFEM.rigid_jointed!(km::Matrix{Float64}, prop::Matrix{Float64}, gamma::Vector{Float64}, etype::Vector{Int}, iel::Int, coord::Matrix{Float64})\nPtFEM.rod_km!(km::Matrix{Float64}, ea::Float64, length::Float64)\nPtFEM.rod_mm!(mm::Matrix{Float64}, length::Float64)\nPtFEM.sample!\nPtFEM.shape_der!(der::Matrix{Float64}, points::Matrix{Float64}, i::Int)\nPtFEM.shape_fun!(fun::Vector{Float64}, points::Matrix{Float64}, i::Int)\nPtFEM.stability(gsm::SparseMatrixCSC{Float64,Int}, ggm::SparseMatrixCSC{Float64,Int}, tol::Float64, limit::Int)"
},

{
    "location": "index.html#PtFEM.geom_rect!",
    "page": "PtFEM.jl documentation",
    "title": "PtFEM.geom_rect!",
    "category": "Function",
    "text": "geom_rect!\n\nThis subroutine forms the coordinates and connectivity for a rectangular mesh of right angled triangular elements (3, 6, 10 or 15-node) or quadrilateral elements (4, 8 or 9-node) counting in the x- or y-dir. \n\nMethod\n\ngeom_rect!(fin_el, iel, x_coords, y_coords, coord, num, dir)\n\nArguments\n\n* fin_el::FiniteElement            : Shape of finite element\n                                     (Trangle or Quadrilateral)\n* iel::Int                       : Element number\n* x_coords::FloatRange{Float64}    : x coordinates\n* y_coords::FloatRange{Float64}    : y coordinates\n* coord::Matrix{Float64}           : Nodal coordinates (Updated)\n* num::Vector{Int}               : Node numbers (Updated)\n* dir::Symbol                      : Node numbering direction\n\n\n\n"
},

{
    "location": "index.html#PtFEM.hexahedron_xz!",
    "page": "PtFEM.jl documentation",
    "title": "PtFEM.hexahedron_xz!",
    "category": "Function",
    "text": "hexahedron_xz!\n\nThis subroutine generates nodal coordinates and numbering for 8, 14 or 20-node \"bricks\" counting x-z planes in the y-direction. \n\nMethod\n\nhexahedron_xz!(iel, x_coords, y_coords, z_coords, coord, num)\n\nArguments\n\n* iel::Int                       : Element number\n* x_coords::FloatRange{Float64}    : x coordinates\n* y_coords::FloatRange{Float64}    : y coordinates\n* z_coords::FloatRange{Float64}    : y coordinates\n* coord::Matrix{Float64}           : Nodal coordinates (Updated)\n* num::Vector{Int}               : Node numbers (Updated)\n* dir::Symbol                      : Node numbering direction\n\n\n\n"
},

{
    "location": "index.html#PtFEM.mesh_size",
    "page": "PtFEM.jl documentation",
    "title": "PtFEM.mesh_size",
    "category": "Function",
    "text": "mesh_size\n\nFunction mesh_size returns the number of fin_els (nels) and the number of nodes (nn) in a 1, 2 or 3-d geometry-created mesh.\n\nMethod\n\n(nels, nn) = mesh_size(fin_el, nxe, [nye[, nze]])\n\nArguments\n\n* fin_el::FiniteElement   : Shape of finite element\n                            1D: Line\n                            2D: Trangle or Quadrilateral\n                            3D: Hexahedron\n* nxe::Int              : Number of fin_els in x direction\n* nye::Int              : Number of fin_els in y direction (for 2D and 3D)\n* nze::Int              : Number of fin_els in z direction (3D only)\n\n\n\nmesh_size\n\nmesh_size: The function mesh_size returns the number of fin_els (nels) and the number of nodes (nn) in a 2-d geometry-created mesh.\n\nMethod\n\n(nels, nn) = mesh_size(fin_el, nxe)\n\nArguments\n\n* `fin_el` : Shape of 2D finite element (Triangle)\n* `nxe` : Number of fin_els in x direction\n* `nxe` : Number of fin_els in y direction\n\n\n\nmesh_size\n\nmesh_size: The function mesh_size returns the number of fin_els (nels) and the number of nodes (nn) in a 2-d geometry-created mesh.\n\nMethod\n\n(nels, nn) = mesh_size(fin_el, nxe, nye)\n\nArguments\n\n* `fin_el` : Shape of 2D finite element (Quadrilateral)\n* `nxe` : Number of fin_els in x direction\n* `nye` : Number of fin_els in y direction\n\n\n\nmesh_size\n\nmesh_size: The function mesh_size returns the number of fin_els (nels) and the number of nodes (nn) in a 3-d geometry-created mesh.\n\nMethod\n\n(nels, nn) = mesh_size(fin_el, nxe, nye, nze)\n\nArguments\n\n* `fin_el` : Shape of 2D finite element (Hexahedron)\n* `nxe` : Number of fin_els in x direction\n* `nye` : Number of fin_els in y direction\n* `nxe` : Number of fin_els in x direction\n\n\n\n"
},

{
    "location": "index.html#PtFEM-Geom-1",
    "page": "PtFEM.jl documentation",
    "title": "PtFEM - Geom",
    "category": "section",
    "text": "PtFEM.geom_rect!\nPtFEM.hexahedron_xz!\nPtFEM.mesh_size"
},

{
    "location": "index.html#PtFEM-Plot-methods-1",
    "page": "PtFEM.jl documentation",
    "title": "PtFEM - Plot methods",
    "category": "section",
    "text": "PtFEM.mesh(data::Dict, g_coord::Array{Float64,2}, g_num::Array{Int, 2}, disp, ampl, pdir)"
},

{
    "location": "index.html#PtFEM.vtk-Tuple{Dict,Any,Any,Any,Any}",
    "page": "PtFEM.jl documentation",
    "title": "PtFEM.vtk",
    "category": "Method",
    "text": "vtk\n\nPlots displacements and directions\n\nFunction\n\nmesh(data, fm_dt, sigma_dt, dir, fname)\n\nArguments\n\n* data::Dict                 : Input dictionary\n* fm_dt::DataTable           : Forces and moments DataTable\n* sigma_dt::DataTable        : Stresses DataTable\n* dir::AbstractString        : Project directory\n* fname::AbstractString      : Output VTK file name\n\n\n\n"
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
    "text": "cholfact(A, [uplo::Symbol,] Val{false}) -> Cholesky\n\nCompute the Cholesky factorization of a dense symmetric positive definite matrix A and return a Cholesky factorization. The matrix A can either be a Symmetric or Hermitian StridedMatrix or a perfectly symmetric or Hermitian StridedMatrix. In the latter case, the optional argument uplo may be :L for using the lower part or :U for the upper part of A. The default is to use :U. The triangular Cholesky factor can be obtained from the factorization F with: F[:L] and F[:U]. The following functions are available for Cholesky objects: size, \\, inv, and det. A PosDefException exception is thrown in case the matrix is not positive definite.\n\nExample\n\njulia> A = [4. 12. -16.; 12. 37. -43.; -16. -43. 98.]\n3×3 Array{Float64,2}:\n   4.0   12.0  -16.0\n  12.0   37.0  -43.0\n -16.0  -43.0   98.0\n\njulia> C = cholfact(A)\nBase.LinAlg.Cholesky{Float64,Array{Float64,2}} with factor:\n[2.0 6.0 -8.0; 0.0 1.0 5.0; 0.0 0.0 3.0]\n\njulia> C[:U]\n3×3 UpperTriangular{Float64,Array{Float64,2}}:\n 2.0  6.0  -8.0\n  ⋅   1.0   5.0\n  ⋅    ⋅    3.0\n\njulia> C[:L]\n3×3 LowerTriangular{Float64,Array{Float64,2}}:\n  2.0   ⋅    ⋅\n  6.0  1.0   ⋅\n -8.0  5.0  3.0\n\njulia> C[:L] * C[:U] == A\ntrue\n\n\n\ncholfact(A, [uplo::Symbol,] Val{true}; tol = 0.0) -> CholeskyPivoted\n\nCompute the pivoted Cholesky factorization of a dense symmetric positive semi-definite matrix A and return a CholeskyPivoted factorization. The matrix A can either be a Symmetric or Hermitian StridedMatrix or a perfectly symmetric or Hermitian StridedMatrix. In the latter case, the optional argument uplo may be :L for using the lower part or :U for the upper part of A. The default is to use :U. The triangular Cholesky factor can be obtained from the factorization F with: F[:L] and F[:U]. The following functions are available for PivotedCholesky objects: size, \\, inv, det, and rank. The argument tol determines the tolerance for determining the rank. For negative values, the tolerance is the machine precision.\n\n\n\ncholfact(A; shift = 0.0, perm = Int[]) -> CHOLMOD.Factor\n\nCompute the Cholesky factorization of a sparse positive definite matrix A. A must be a SparseMatrixCSC, Symmetric{SparseMatrixCSC}, or Hermitian{SparseMatrixCSC}. Note that even if A doesn't have the type tag, it must still be symmetric or Hermitian. A fill-reducing permutation is used. F = cholfact(A) is most frequently used to solve systems of equations with F\\b, but also the methods diag, det, and logdet are defined for F. You can also extract individual factors from F, using F[:L]. However, since pivoting is on by default, the factorization is internally represented as A == P'*L*L'*P with a permutation matrix P; using just L without accounting for P will give incorrect answers. To include the effects of permutation, it's typically preferable to extract \"combined\" factors like PtL = F[:PtL] (the equivalent of P'*L) and LtP = F[:UP] (the equivalent of L'*P).\n\nSetting the optional shift keyword argument computes the factorization of A+shift*I instead of A. If the perm argument is nonempty, it should be a permutation of 1:size(A,1) giving the ordering to use (instead of CHOLMOD's default AMD ordering).\n\nnote: Note\nThis method uses the CHOLMOD library from SuiteSparse, which only supports doubles or complex doubles. Input matrices not of those element types will be converted to SparseMatrixCSC{Float64} or SparseMatrixCSC{Complex128} as appropriate.Many other functions from CHOLMOD are wrapped but not exported from the Base.SparseArrays.CHOLMOD module.\n\n\n\n"
},

{
    "location": "index.html#Base.:\\",
    "page": "PtFEM.jl documentation",
    "title": "Base.:\\",
    "category": "Function",
    "text": "\\(x, y)\n\nLeft division operator: multiplication of y by the inverse of x on the left. Gives floating-point results for integer arguments.\n\njulia> 3 \\ 6\n2.0\n\njulia> inv(3) * 6\n2.0\n\njulia> A = [1 2; 3 4]; x = [5, 6];\n\njulia> A \\ x\n2-element Array{Float64,1}:\n -4.0\n  4.5\n\njulia> inv(A) * x\n2-element Array{Float64,1}:\n -4.0\n  4.5\n\n\n\n\\(A, B)\n\nMatrix division using a polyalgorithm. For input matrices A and B, the result X is such that A*X == B when A is square. The solver that is used depends upon the structure of A.  If A is upper or lower triangular (or diagonal), no factorization of A is required and the system is solved with either forward or backward substitution. For non-triangular square matrices, an LU factorization is used.\n\nFor rectangular A the result is the minimum-norm least squares solution computed by a pivoted QR factorization of A and a rank estimate of A based on the R factor.\n\nWhen A is sparse, a similar polyalgorithm is used. For indefinite matrices, the LDLt factorization does not use pivoting during the numerical factorization and therefore the procedure can fail even for invertible matrices.\n\nExample\n\njulia> A = [1 0; 1 -2]; B = [32; -4];\n\njulia> X = A \\ B\n2-element Array{Float64,1}:\n 32.0\n 18.0\n\njulia> A * X == B\ntrue\n\n\n\n"
},

{
    "location": "index.html#PtFEM-Julia-functions-and-operators-1",
    "page": "PtFEM.jl documentation",
    "title": "PtFEM - Julia functions & operators",
    "category": "section",
    "text": "cholfact\n\\"
},

{
    "location": "index.html#Base.Distributed.pmap",
    "page": "PtFEM.jl documentation",
    "title": "Base.Distributed.pmap",
    "category": "Function",
    "text": "pmap([::AbstractWorkerPool], f, c...; distributed=true, batch_size=1, on_error=nothing, retry_delays=[]), retry_check=nothing) -> collection\n\nTransform collection c by applying f to each element using available workers and tasks.\n\nFor multiple collection arguments, apply f elementwise.\n\nNote that f must be made available to all worker processes; see Code Availability and Loading Packages for details.\n\nIf a worker pool is not specified, all available workers, i.e., the default worker pool is used.\n\nBy default, pmap distributes the computation over all specified workers. To use only the local process and distribute over tasks, specify distributed=false. This is equivalent to using asyncmap. For example, pmap(f, c; distributed=false) is equivalent to asyncmap(f,c; ntasks=()->nworkers())\n\npmap can also use a mix of processes and tasks via the batch_size argument. For batch sizes greater than 1, the collection is processed in multiple batches, each of length batch_size or less. A batch is sent as a single request to a free worker, where a local asyncmap processes elements from the batch using multiple concurrent tasks.\n\nAny error stops pmap from processing the remainder of the collection. To override this behavior you can specify an error handling function via argument on_error which takes in a single argument, i.e., the exception. The function can stop the processing by rethrowing the error, or, to continue, return any value which is then returned inline with the results to the caller.\n\nConsider the following two examples. The first one returns the exception object inline, the second a 0 in place of any exception:\n\njulia> pmap(x->iseven(x) ? error(\"foo\") : x, 1:4; on_error=identity)\n4-element Array{Any,1}:\n 1\n  ErrorException(\"foo\")\n 3\n  ErrorException(\"foo\")\n\njulia> pmap(x->iseven(x) ? error(\"foo\") : x, 1:4; on_error=ex->0)\n4-element Array{Int64,1}:\n 1\n 0\n 3\n 0\n\nErrors can also be handled by retrying failed computations. Keyword arguments retry_delays and retry_check are passed through to retry as keyword arguments delays and check respectively. If batching is specified, and an entire batch fails, all items in the batch are retried.\n\nNote that if both on_error and retry_delays are specified, the on_error hook is called before retrying. If on_error does not throw (or rethrow) an exception, the element will not be retried.\n\nExample: On errors, retry f on an element a maximum of 3 times without any delay between retries.\n\npmap(f, c; retry_delays = zeros(3))\n\nExample: Retry f only if the exception is not of type InexactError, with exponentially increasing delays up to 3 times. Return a NaN in place for all InexactError occurrences.\n\npmap(f, c; on_error = e->(isa(e, InexactError) ? NaN : rethrow(e)), retry_delays = ExponentialBackOff(n = 3))\n\n\n\n"
},

{
    "location": "index.html#PtFEM-Parallel-processing-1",
    "page": "PtFEM.jl documentation",
    "title": "PtFEM - Parallel processing",
    "category": "section",
    "text": "pmap"
},

{
    "location": "index.html#PtFEM.fkdiag!",
    "page": "PtFEM.jl documentation",
    "title": "PtFEM.fkdiag!",
    "category": "Function",
    "text": "fkdiag!\n\nThis subroutine returns the elastic dee matrix for ih=3 (plane strain), ih=4 (axisymmetry or plane strain elastoplasticity) or ih=6 (three dimensions).\n\nMethod\n\nfkdiag!(kdiag, g)\n\nArguments\n\n* kdiag::Vector{Int}      : Bandwidth vector (Updated)\n* g::Vector{Int}          : Element steering vector\n\n\n\n"
},

{
    "location": "index.html#PtFEM.fromSkyline-Tuple{Array{Float64,1},Array{Int64,1}}",
    "page": "PtFEM.jl documentation",
    "title": "PtFEM.fromSkyline",
    "category": "Method",
    "text": "fromSkyline\n\nHelper function to convert a Skyline vector to a full matrix.\n\nType\n\nfromSkyline(skyline::Vector{Float64}, kdiag::Vector{Int})\n\nArguments\n\n* skyline::Vector{Float64}     : 1D Line(nod, nodof)\n* kdiag::Vector{Int}         : 2D Triangle(nod, nodof)\n\n\n\n"
},

{
    "location": "index.html#PtFEM.fsparv!-Tuple{Array{Float64,1},Array{Float64,2},Array{Int64,1},Array{Int64,1}}",
    "page": "PtFEM.jl documentation",
    "title": "PtFEM.fsparv!",
    "category": "Method",
    "text": "fsparv!\n\nFunction fsparv! assembles fin_el matrices into a symmetric skyline global matrix. The Skyline vector kv is updated.\n\nMethod\n\nfsparv!(kv, km, g, km)\n\nArguments\n\n* kv::Vector{Float64}        : Sparse stiffnes matrix (Updated)\n* km::Matrix{Float64}        : Symmetric element stiffnes matrix\n* g::Vector{Int}           : Global steering vector.\n* kdiag::Vector{Int}       : Location of diagoinal terms\n\n\n\n"
},

{
    "location": "index.html#PtFEM.linmul_sky!",
    "page": "PtFEM.jl documentation",
    "title": "PtFEM.linmul_sky!",
    "category": "Function",
    "text": "linmul_sky!\n\nThis subroutine forms the product of symmetric matrix stored as a skyline and a vector.\n\nMethod\n\nlinmul_sky!(kv, disps, loads, kdiag)\n\nArguments\n\n* kv::Vector{Float64}       : Sparse stiffnes matrix (Skyline format)\n* disps::Vector{Float64}    : Displacements\n* loads::Vector{Float64}    : Loads vector (Updated)\n* kdiag::Vector{Int}      : Bandwidth vector\n\n\n\n"
},

{
    "location": "index.html#PtFEM.skyline2sparse-Tuple{Array{Float64,1},Array{Int64,1}}",
    "page": "PtFEM.jl documentation",
    "title": "PtFEM.skyline2sparse",
    "category": "Method",
    "text": "skyline2sparse\n\nConverts a Skyline matrix to a Julia Sparse matrix\n\nFunction\n\nskyline2sparse(skyline, kdiag)\n\nArguments\n\n* skyline::Vector{Float64}         : Skyline matrix\n* kdiag::Vector{Int}             : Element diagonal index vector\n\n\n\n"
},

{
    "location": "index.html#PtFEM.spabac!-Tuple{Array{Float64,1},Array{Float64,1},Array{Int64,1}}",
    "page": "PtFEM.jl documentation",
    "title": "PtFEM.spabac!",
    "category": "Method",
    "text": "spabac!\n\nFunction spabac! performs Cholesky forward and back-substitution on a symmetric skyline global matrix. The loads vector is updated.\n\n###Arguments\n\nspabac!(kv, loads, kdiag)\n\nArguments\n\n* kv::Vector{Float64}       : Skyline vector of global stiffness matrix\n* loads::Vector{Float64}    : Load vector (Updated)\n* kdiag::Vector{Int}      : Diagonal elemnt index vector\n\n\n\n"
},

{
    "location": "index.html#PtFEM.sparin!-Tuple{Array{Float64,1},Array{Int64,1}}",
    "page": "PtFEM.jl documentation",
    "title": "PtFEM.sparin!",
    "category": "Method",
    "text": "sparin!\n\nFunction sparin! performs Cholesky factorisation on a symmetric skyline global matrix. The vector kv is updated.\n\n###Arguments\n\nsparin!(kv, kdiag)\n\nArguments\n\n* kv::Vector{Float64}       : Global stiffness matrix (Updated)\n* kdiag::Vector{Int}      : Diagonal elemnt index vector\n\n\n\n"
},

{
    "location": "index.html#PtFEM-Deprecated-1",
    "page": "PtFEM.jl documentation",
    "title": "PtFEM - Deprecated",
    "category": "section",
    "text": "PtFEM.fkdiag!\nPtFEM.fromSkyline(skyline::Vector{Float64}, kdiag::Vector{Int})\nPtFEM.fsparv!(kv::Vector{Float64}, km::Matrix{Float64}, g::Vector{Int}, kdiag::Vector{Int})\nPtFEM.linmul_sky!\nPtFEM.skyline2sparse(skyline::Vector{Float64}, kdiag::Vector{Int})\nPtFEM.spabac!(kv::Vector{Float64}, loads::Vector{Float64}, kdiag::Vector{Int})\nPtFEM.sparin!(kv::Vector{Float64}, kdiag::Vector{Int})"
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
    "text": "Complete inital framework, including documentation (May 2017)\nRemaining programs in chapter 6 (June 2017)\nReview and complete plot recipes for chapters 4 to 6 (June 2017)\nRework chapters 5 and 6 for Julia sparse matrices (May 2017)\nUpdate notebooks (July 2017)\nAdd generalized WriteVTK.jl recipes\nProfile p5. and p6. programs (summer 2017?)\nComplete chapter 7\nComplete chapter 8 (Sep 2017)\nComplete chapter 9 (fall 2017?)\nComplete chapter 10\nComplete chapter 11\nComplete chapter 12 (late 2017?)\nPort BHATheoreticalPerformance from Fortran/R/Julia to using PtFEM.jl  (early 2018?)\nPort BHALockup from Fortran/R to using PtFEM.jl (mid 2018?)\nInvestigate if I could use JuaFEM components"
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
