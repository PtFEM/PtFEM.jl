function useplots()
  res1 = ""
  try
  	res = Pkg.installed("Plots")
  catch
    println("Package Plots not installed, please run Pkg.add(\"Plots\")")
    return
  end

  res2 = ""
  try
  	res = Pkg.installed("GR")
  catch
    println("Package GR.jl not installed, please run Pkg.add(\"GR\")")
    return
  end
  
  if typeof(res1) == VersionNumber && typeof(res2) == VersionNumber
    eval(quote
      using Plots
      using GR
    end)
  end
  typeof(res1) == VersionNumber && typeof(res2) == VersionNumber
end