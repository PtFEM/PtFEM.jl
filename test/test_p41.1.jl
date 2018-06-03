using PtFEM
using Test: @test

data = Dict(
  # Rod(nels, np_types, nip, fin_el(nod, nodof))
  :struc_el => Rod(4, 1, 1, Line(2, 1)),
  :properties => [1.0e5;],
  :x_coords => range(0, stop=1, length=5),
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

@time m, dis_fm, fm_df = p41(data)

println("\nTesting: round.(m.displacements, digits=8) == [-2.5e-5 -2.344e-5 -1.875e-5 -1.094e-5 0.0]'\n")

@test round.(m.displacements, digits=8) == [-2.5e-5 -2.344e-5 -1.875e-5 -1.094e-5 0.0]'
