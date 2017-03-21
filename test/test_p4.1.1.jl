using CSoM, Base.Test.@test

data = Dict(
  # Rod(nels, np_types, nip, fin_el(nod, nodof))
  :struc_el => Rod(4, 1, 1, Line(2, 1)),
  :properties => [1.0e5;],
  :x_coords => linspace(0, 1, 5),
  :support => [
    (5, [0])
    ],
  :loaded_nodes => [
    (1, [-0.625]),
    (2, [-1.25]),
    (3, [-1.25]),
    (4, [-1.25]),
    (5, [-0.625])
    ]
)

@time m = p4_1(data)

@test round.(m.displacements, 8) == [-2.5e-5 -2.344e-5 -1.875e-5 -1.094e-5 0.0]'
