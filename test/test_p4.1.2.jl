using CSoM

data = Dict(
  # Rod(nels, np_types, nip, finite_element(nod, nodof))
  :struc_el => Rod(4, 2, 1, Line(2, 1)),
  :properties => [2.0e3; 1.0e3],
  :etype => [2, 2, 1, 1],
  :x_coords => linspace(0, 1, 5),
  :support => [
    (1, [0])
    ],
  :fixed_freedoms => [
    (5, 1, 0.05)
    ]
)

@time m = p4_1(data)

@test round.(m.displacements, 7) == [0.0 0.0166667 0.0333333 0.0416667 0.05]'
