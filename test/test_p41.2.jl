using PtFEM
using Test: @test

data = Dict(
  # Rod(nels, np_types, nip, finite_element(nod, nodof))
  :struc_el => Rod(4, 2, 1, Line(2, 1)),
  :properties => [2.0e3; 1.0e3],
  :etype => [2, 2, 1, 1],
  :x_coords => range(0, stop=1, length=5),
  :support => [
    (1, [0])
    ],
  :fixed_freedoms => [
    (5, 1, 0.05)
    ]
)

@time fem, dis_df, fm_df = p41(data)

println("\nTesting: round.(fem.displacements, digits=7) == [0.0 0.0166667 0.0333333 0.0416667 0.05]' \n")

@test round.(fem.displacements, digits=7) == [0.0 0.0166667 0.0333333 0.0416667 0.05]'
