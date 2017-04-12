using Base.Test, PtFEM

data = Dict(
  # Plane(ndim, nst, nxe, nye, nip, direction, finite_element(nod, nodof), axisymmetric)
  :struc_el => Plane(2, 3, 20, 2, 4, :y, Quadrilateral(8, 2), false),
  :properties => [1.0e5 0.0 1.0e-5 1.0e-5;],
  :x_coords => collect(linspace(0.0, 1.0, 21)),
  :y_coords => collect(linspace(0.05, -0.05, 3)),
  :temp => [],
  :support => [
    (1, [0 0]),
    (2, [0 0]),
    (3, [0 0]),
    (4, [0 0]),
    (5, [0 0])
  ],
  :dtemp => [
    0.50, 0.25, 0.00, -0.25, -0.50, 0.50, 0.00, -0.50,
    0.50, 0.25, 0.00, -0.25, -0.50, 0.50, 0.00, -0.50,
    0.50, 0.25, 0.00, -0.25, -0.50, 0.50, 0.00, -0.50,
    0.50, 0.25, 0.00, -0.25, -0.50, 0.50, 0.00, -0.50,
    0.50, 0.25, 0.00, -0.25, -0.50, 0.50, 0.00, -0.50,
    0.50, 0.25, 0.00, -0.25, -0.50, 0.50, 0.00, -0.50,
    0.50, 0.25, 0.00, -0.25, -0.50, 0.50, 0.00, -0.50,
    0.50, 0.25, 0.00, -0.25, -0.50, 0.50, 0.00, -0.50,
    0.50, 0.25, 0.00, -0.25, -0.50, 0.50, 0.00, -0.50,
    0.50, 0.25, 0.00, -0.25, -0.50, 0.50, 0.00, -0.50,
    0.50, 0.25, 0.00, -0.25, -0.50, 0.50, 0.00, -0.50,
    0.50, 0.25, 0.00, -0.25, -0.50, 0.50, 0.00, -0.50,
    0.50, 0.25, 0.00, -0.25, -0.50, 0.50, 0.00, -0.50,
    0.50, 0.25, 0.00, -0.25, -0.50, 0.50, 0.00, -0.50,
    0.50, 0.25, 0.00, -0.25, -0.50, 0.50, 0.00, -0.50,
    0.50, 0.25, 0.00, -0.25, -0.50, 0.50, 0.00, -0.50,
    0.50, 0.25, 0.00, -0.25, -0.50, 0.50, 0.00, -0.50,
    0.50, 0.25, 0.00, -0.25, -0.50, 0.50, 0.00, -0.50,
    0.50, 0.25, 0.00, -0.25, -0.50, 0.50, 0.00, -0.50,
    0.50, 0.25, 0.00, -0.25, -0.50, 0.50, 0.00, -0.50,
    0.50, 0.25, 0.00, -0.25, -0.50
  ],
  :nspr => [
    (163, 2, 50.0)
  ]
)

@time m = p55(data)

@test round(m.loads[1:20], 6) ≈ round([0.0,-9.088858462889305e-10,7.925850199690076e-8,
  -5.429946419597694e-21,-4.382299263408736e-8,9.088858462780911e-10,
  7.925850199690076e-8,1.296940403436091e-9,9.125889568564588e-8,
  6.4039863087481865e-9,-4.512138001498236e-9,-1.0854382987425028e-20,
  -2.1036347161742548e-8,-6.403986308769904e-9,-4.512138001498197e-9,
  -1.2969404034577835e-9,9.125889568564593e-8,1.1885079699791057e-8,
  9.585609222810678e-8,-1.6280098183907838e-20], 6)