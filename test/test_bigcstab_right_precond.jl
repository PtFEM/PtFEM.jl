using CSoM

a = Float64[10 1 -5; -20 3 20; 5 3 5]
b = Float64[1; 2; 6]
x = Float64[1; 1; 1]

bicgstab!(a, b, x)
