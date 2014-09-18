module LHR

  # package code goes here
  ### Imports ###
  
  ### Includes ###
  
  include("csomif/if.jl")
  include("FEmodel.jl")
  include("fem.jl")
  
  ### Exports ###
  
  export
    FEmodel,
    fem

  ### Deprecated ###
    
    include("deprecated.jl")

end # module
