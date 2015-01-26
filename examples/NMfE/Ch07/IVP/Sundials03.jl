using Sundials

## f2 routine. Compute function f(x,y).

function f(x, y, ydot)
    ydot[1] = y[2]
    ydot[2] = -2.0*y[2]^2 / y[1]
end
x = linspace(0.0, 1.0, 11)
@show Sundials.cvode(f, [1.0, 1.0], x)
