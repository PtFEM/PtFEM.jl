using Sundials, Gadfly

old = pwd()
ProjDir = Pkg.dir("CSoM", "examples", "NMfE", "Ch07")
cd(ProjDir)

## f routine. Compute function f(t,y).

function f(t, y, ydot)
    ydot[1] = -0.04*y[1] + 1.0e4*y[2]*y[3]
    ydot[3] = 3.0e7*y[2]*y[2]
    ydot[2] = -ydot[1] - ydot[3]
end
#t = [0.0, 4 * logspace(-1., 7., 9)]
t = linspace(0.0, 100.0, 150)
res = Sundials.cvode(f, [1.0, 0.1, 0.0], t)

p = plot(
  layer(x=t, y=res[:,1], Geom.line,
    color=repeat([symbol("y[1]")], inner=[4])),
  layer(x=t, y=res[:,2], Geom.line,
    color=repeat([symbol("y[2]")], inner=[2])),
  layer(x=t, y=res[:,3], Geom.line,
    color=repeat([symbol("y[3]")], inner=[1])),
  Scale.discrete_color_manual("darkred", "red", "green"),
  Guide.colorkey("Legend"),
  Guide.xlabel("x", orientation=:horizontal),
  Guide.ylabel("y[1], y[2] and y[3]", orientation=:vertical),
  Guide.title("Example 7.5 (from SunDials.jl)")
)

draw(SVG("Ex7.5.svg", 8inch, 3inch), p)
# Below will only work on OSX, please adjust for your environment.
# JULIA_SVG_BROWSER is set from environment variable JULIA_SVG_BROWSER
@osx ? if isdefined(Main, :JULIA_SVG_BROWSER) && length(JULIA_SVG_BROWSER) > 0
  isfile("Ex7.5.svg") &&
    run(`open -a $(JULIA_SVG_BROWSER) "Ex7.5.svg"`)
  end : println()

cd(old)