module LHR

  # package code goes here
  ### Imports ###
  
  ### Includes ###
  
  include("csomif/if.jl")
  include("FEmodel.jl")
  
  ### Exports ###
  
  export
    FEmodel
  
  ### Deprecated ###
    
    include("deprecated.jl")

end # module
