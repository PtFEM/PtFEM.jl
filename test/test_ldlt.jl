using CSoM

A = Float64[16 4 8;4 5 -4;8 -4 22]
b = Float64[4, 2, 5]

(L, U) = lufac(A)
y = L\b
@assert y == [4.0,1.0,4.5]
