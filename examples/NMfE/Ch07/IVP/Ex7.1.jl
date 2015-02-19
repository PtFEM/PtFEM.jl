using CSoM, Gadfly

old = pwd()
ProjDir = Pkg.dir("CSoM", "examples", "NMfE", "Ch07", "IVP")
cd(ProjDir)

f(x::Float64, y::Vector{Float64}) = (x + y[1])/x
steps = 10
h = 0.1

x = 2.0
y = [2.0]

# y(3.0) = 4.2165

r = Array{Float64,2}[]
push!(r, euler(f, x, y, steps, h))
push!(r, modified_euler(f, x, y, steps, h))
push!(r, mid_point_euler(f, x, y, steps, h))
push!(r, runga_kutta_4(f, x, y, steps, h))
println(r)

xs = Float64[(i-1)*h for i in 1:steps+1]
titles = ["Euler", "Modified_Euler", "Mid_Point_Euler", "Runga_Kutta_4"]

p = plot(
  layer(x=xs, y=r[1][:, 2], Geom.line,
    color=repeat([symbol(titles[1])], inner=[1])),
  layer(x=xs, y=r[2][:, 2], Geom.line,
    color=repeat([symbol(titles[2])], inner=[2])),
  layer(x=xs, y=r[3][:, 2], Geom.line,
    color=repeat([symbol(titles[3])], inner=[3])),
  layer(x=xs, y=r[4][:, 2], Geom.line,
    color=repeat([symbol(titles[4])], inner=[4])),
  Scale.color_discrete_manual("darkred", "red", "darkblue","darkgreen"),
  Guide.colorkey("Legend"),
  Guide.xlabel("x", orientation=:horizontal),
  Guide.ylabel("y", orientation=:vertical),
  Guide.title("Four ODE one-step methods on y'=(x + y)/x"))

draw(SVG("Ex7.1.svg", 8inch, 9.5inch), p)
# Below will only work on OSX, please adjust for your environment.
# JULIA_SVG_BROWSER is set from environment variable JULIA_SVG_BROWSER
@osx ? if isdefined(Main, :JULIA_SVG_BROWSER) && length(JULIA_SVG_BROWSER) > 0
  isfile("Ex7.1.svg") &&
    run(`open -a $(JULIA_SVG_BROWSER) "Ex7.1.svg"`)
  end : println()

