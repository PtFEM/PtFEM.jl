g(x::Float64, a::Float64, b::Float64) = (a+b)/2 + (b-a)*x/2
scal(a, b) = (b - a)/2

x1, w1 = Base.gauss(Float64, 2)
x2, w2 = Base.gauss(Float64, 7)

f(x) = sin(x)
f1 = [f(g(xi, pi/4, pi/2)) for xi in x1]

@show f1
@show x1
@show w1
println()

@show scal(pi/4, pi/2)*dot(f1, w1)
println()

f2 = [f(g(xi, pi/4, pi/2)) for xi in x2]

@show f2
@show x2
@show w2
println()

@show scal(pi/4, pi/2)*dot(f2, w2)

Base.quadgk(f, pi/4, pi/2)