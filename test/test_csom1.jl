using LHR

data = {
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
}

data |> display
println()

m = FEmodel(20, 21, data)

println()
m.nf |> display
println()

println("m.kdiag (reshaped from $(typeof(m.kdiag)) of length $(size(m.kdiag, 1))):")
reshape(m.kdiag, 20,6) |> display
println()

round(m.km) |> display
println()

println("First 10 elements of updated m.kv (of type $(typeof(m.kv)) with length $(size(m.kv, 1))):")
m.kv[1:10] |> display
println()

println("Displacements:")
m.displacements |> display
println()

println("Actions:")
m.actions |> display
println()

