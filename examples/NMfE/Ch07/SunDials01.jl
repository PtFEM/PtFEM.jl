using Sundials

## f2 routine. Compute function f(x,y).

function f2(x, y, ydot)
    ydot[1] = y[2]
    ydot[2] = 2.0*y[1] - 3.0*y[2] + 3.0*x^2
end
x = linspace(0.0, 0.2, 5)
@show Sundials.cvode(f2, [1.0, 0.0], x)

## Define the system residual function.
function resrob(tres, y, yp, r)
    r[1]  = -0.04*y[1] + 1.0e4*y[2]*y[3]
    r[2]  = -r[1] - 3.0e7*y[2]*y[2] - yp[2]
    r[1] -=  yp[1]
    r[3]  =  y[1] + y[2] + y[3] - 1.0
end

t = [0.0, 4 * logspace(-1., 5., 7)]
yout, ypout = Sundials.idasol(resrob, [1.0, 0, 0], [-0.04, 0.04, 0.0], t)
@show yout ypout

function sysfn(y, fy)
    fy[1] = y[1]^2 + y[2]^2 - 1.0
    fy[2] = y[2] - y[1]^2
end

res = Sundials.kinsol(sysfn, ones(2))

@assert res = [0.786153, 0.618035]
