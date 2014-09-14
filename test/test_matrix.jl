isfile("mat.so") && rm("mat.so")

run(`gfortran matrix.f95 matrix2.f95 -o mat.so -shared -fPIC`)

matrix = dlopen("mat.so")
vec_ = dlsym(matrix,:vec_)
mat_ = dlsym(matrix,:mat_)
tten_ = dlsym(matrix,:tten_)

a = linspace(1.,4.,4)
ccall(vec_,Void,(Ptr{Int64},Ptr{Float64}),&int64(4),a)
println()
a |> display

a = linspace(1.,4.,4)'
ccall(vec_,Void,(Ptr{Int64},Ptr{Float64}),&int64(4),a)
println()
a |> display
println()

b = ones(Float64,4,1)
c = b * a
c |> display
ccall(mat_,Void,(Ptr{Int64},Ptr{Int64},Ptr{Float64}),&int64(4),&int64(4),c)
println()
c |> display

d = ones(Float64,4,4,2)
d[:,:,1] = c
ccall(tten_,Void,(Ptr{Int64},Ptr{Int64},Ptr{Int64},Ptr{Float64}),
  &int64(4),&int64(4),&int64(2),d)
println()
d |> display

