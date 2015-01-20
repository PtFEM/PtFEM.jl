using Compat, CSoM

data = @Compat.Dict(
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
    (21, [10000.0 0.0 0.0 0.0 0.0 0.0])]
)

data |> display
println()

m = FEmodel(20, 21, data)

println("Displacements:")
m.displacements |> display
println()

println("Actions:")
m.actions |> display
println()
