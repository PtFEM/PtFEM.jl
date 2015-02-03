using Compat, CSoM

#=
'plane'
'triangle'   3   'x'
2  2  1  1
1.0e6  0.3
0.0  0.5  1.0
0.0 -0.5 -1.0
5
1 0 1  4 0 1  7 0 0  8 1 0  9 1 0
3
1  0.0 -0.25   2  0.0 -0.50   3  0.0 -0.25
0
=#

data = @compat Dict(
  :type => :plane,
  :element => :triangle,
  :nod => 3,
  :direction => :x,
  :nxe => 2,
  :nye => 2,
  :nip => 1,
  :support => [
    (1, [0 0 0 0 0 0])],
  :properties => [
    (1, [1.0e6 0.3])],
  :coordinates => [
    (1, linspace(0, 4, 21))],
  :node_numbering => [
    (1, int(linspace(1, 20, 20))),
    (2, int(linspace(2, 21, 20)))],
  :loads => [
    (21, [10000.0 1000.0 0.0 1000.0 0.0 0.0])]
)

data |> display
println()

@time m = FEplate(20, 21, data)
println()
