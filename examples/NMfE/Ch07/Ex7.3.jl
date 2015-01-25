using CSoM, Gadfly

old = pwd()
ProjDir = Pkg.dir("CSoM", "examples", "NMfE", "Ch07")
cd(ProjDir)

f(x::Float64, y::Vector{Float64}) = [y[2], 2.0*y[1]-3.0*y[2]+3.0*x^2]
steps = 5
h = 0.05

x = 0.0
y = [1.0, 0.0]


r = Array{Float64,2}[]
push!(r, euler(f, x, y, steps, h))
push!(r, modified_euler(f, x, y, steps, h))
push!(r, mid_point_euler(f, x, y, steps, h))
push!(r, runga_kutta_4(f, x, y, steps, h))
println(r)

xs = Float64[(i-1)*h for i in 1:steps]
pa = Plot[]
titles = ["Euler", "Modified_Euler", "Mid_Point_Euler", "Runga_Kutta_4"]
for i in 1:4
  y1 = r[i][:, 2]
  y2 = r[i][:, 3]
  p = plot(
    layer(x=xs, y=y1, Geom.line,
      color=repeat([symbol("y1")], inner=[3])),
    layer(x=xs, y=y2, Geom.line,
      color=repeat([symbol("y2")], inner=[1])),
    Scale.discrete_color_manual("darkred", "red", "darkblue","darkgreen"),
    Guide.colorkey("Legend"),
    Guide.xlabel("x", orientation=:horizontal),
    Guide.ylabel("w(x)", orientation=:vertical),
    Guide.title(titles[i]))
  push!(pa, p)
end

draw(SVG("Ex7.3.svg", 8inch, 9.5inch), vstack(pa...))
# Below will only work on OSX, please adjust for your environment.
# JULIA_SVG_BROWSER is set from environment variable JULIA_SVG_BROWSER
@osx ? if isdefined(Main, :JULIA_SVG_BROWSER) && length(JULIA_SVG_BROWSER) > 0
  isfile("Ex7.3.svg") &&
    run(`open -a $(JULIA_SVG_BROWSER) "Ex7.3.svg"`)
  end : println()

cd(old)