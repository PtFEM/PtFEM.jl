# nf data input file format
#=    
       1 0 0 1       2 1 0 1       3 1 0 1       4 1 0 1       5 1 0 1
       6 1 0 1       7 1 0 1       8 1 0 1       9 1 0 1      10 1 0 1
      11 1 0 1      12 1 0 1      13 1 0 1      14 1 0 1      15 1 0 1
      ...
=#


function format_nf_line!(l::String, t::Array{Tuple})
  sl = int(split(l))
  @assert mod(length(sl), 4) == 0
  sl
end

function read_nf_file(f::String)
  f = open(f)
  t = Tuple[]
  while !eof(f)
    l = readline(f)
    sl = format_nf_line!(l, t)
    for k in 1:length(sl)/4
      ind = (k-1)*4
      append!(t, [(sl[ind+1], [sl[ind+2] sl[ind+3] sl[ind+4]])])
    end
  end
  close(f)
  t
end


#loads data input file format
#=
    1  0.00000000E+00  0.00000000E+00  0.10416667E-03
    2  0.00000000E+00  0.00000000E+00 -0.41666667E-03
    3  0.00000000E+00  0.00000000E+00  0.20833333E-03
    4  0.00000000E+00  0.00000000E+00 -0.41666667E-03
    ...
=#

function format_loads_line!(l::String, t::Array{Tuple})
  sl = float(split(l))
  @assert mod(length(sl), 4) == 0
  sl
end

function read_loads_file(f::String)
  f = open(f)
  t = Tuple[]
  while !eof(f)
    l = readline(f)
    sl = format_loads_line!(l, t)
    for k in 1:length(sl)/4
      ind = (k-1)*4
      append!(t, [(int(sl[ind+1]), [sl[ind+2] sl[ind+3] sl[ind+4]])])
    end
  end
  close(f)
  t
end
