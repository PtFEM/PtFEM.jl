using CSoM, Compat

data = @compat Dict(
  :support => [
    (1, [0 0 0 0 0 0])],
  :properties => [
    (1, [2.0e6 1.0e6 1.0e6 3.0e5])],
  :coordinates => [
    (1, linspace(0, 4, 21))],
  :node_numbering => [
    (1, int(linspace(1, 20, 20))),
    (2, int(linspace(2, 21, 20)))],
  :loads => [
    (21, [0.0 -10000.0 0.0 0.0 0.0 0.0])]
)

m = FEmodel(20, 21, data)

@assert round(m.displacements[2,1:7], 5) == [0.0  -0.00079  -0.00309  -0.00684  -0.01195  -0.01833  -0.02592]
@assert round(m.displacements[2,8:14], 5) == [-0.03463  -0.04437  -0.05508  -0.06667  -0.07905  -0.09216  -0.10591]
@assert round(m.displacements[2,15:21], 5) == [-0.12021  -0.135  -0.15019  -0.16569  -0.18144  -0.19735  -0.21333]
