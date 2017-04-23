using PtFEM

data = Dict(
  # Frame(nels, nn, ndim, nst, nip, finite_element(nod, nodof))
  :struc_el => Frame(20, 21, 3, 1, 1, Line(2, 3)),
  :properties => [2.0e6 1.0e6 1.0e6 3.0e5;],
  :x_coords => linspace(0, 4, 21),
  :y_coords => zeros(21),
  :z_coords => zeros(21),
  :g_num => [
    collect(1:20)';
    collect(2:21)'],
  :support => [
    (1, [0 0 0 0 0 0])
    ],
  :loaded_nodes => [
    (21, [10000.0 1000.0 0.0 1000.0 0.0 0.0])],
  :penalty => 1e19
)

m, dis_dt, fm_dt = p44(data)

@assert Vector(round.(m.displacements[2,1:7][:], 5)) == 
  [0.0,  8.0e-5,  0.00031,  0.00068,  0.00119,  0.00183,  0.00259]
