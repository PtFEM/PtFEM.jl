function bc_rect!(nxe,nye,nf,dir)
  #
  # This subroutine generates the nf array for a rectangular mesh
  # of 8-node quadrilaterals fully fixed on the base and with
  # vertical rollers on the left and right sides. Nodes numbered
  # in the x- or y-direction
  #
 if dir == :y          
   nm = 0
   ic = 0
   for j in 1:2nye
     nm = nm+1
     nf[1, nm] = 0
     ic = ic + 1
     nf[2, nm]=ic
   end
   nm += 1
   nf[1, nm] = 0
   nf[2, nm] = 0

   for i in 1:nxe-1
     for j in 1:nye
       nm = nm + 1
       ic = ic + 1
       nf[1, nm] = ic
       ic += 1
       nf[2, nm] = ic  
     end
     nm += 1
     nf[1, nm] = 0
     nf[2, nm] = 0   

     for j in 1:2nye
       nm += 1
       ic += 1
       nf[1, nm] = ic
       ic += 1
       nf[2, nm] = ic
     end
     nm += 1
     nf[1, nm] = 0
     nf[2, nm] = 0
   end
  
   for j in 1:nye
     nm += 1
     ic += 1
     nf[1, nm] = ic
     ic += 1
     nf[2, nm] = ic  
   end
   nm += 1
   nf[1, nm] = 0
   nf[2, nm] = 0   
  
   for j in 1:2nye
     nm += 1
     nf[1, nm] = 0
     ic += 1
     nf[2, nm] = ic
   end
   nm += 1
   nf[1, nm] = 0
   nf[2, nm] = 0
 else
   nm=0
   ic=0
   for j in 1:nye
     nm += 1
     nf[1, nm] = 0
     ic += 1
     nf[2, nm] = ic
     for i in 1:2nxe-1
       nm += 1
       ic += 1
       nf[1, nm] = ic
       ic += 1
       nf[2, nm] = ic  
     end
     nm += 1
     nf[1, nm] = 0
     ic += 1
     nf[2, nm] = ic

     nm += 1
     nf[1, nm] = 0
     ic += 1
     nf[2, nm] = ic
     for i in 1:nxe-1
       nm += 1
       ic += 1
       nf[1, nm] = ic
       ic += 1
       nf[2, nm] = ic  
     end
     nm += 1
     nf[1, nm] = 0
     ic += 1
     nf[2, nm] = ic
   end
   
   for i in 1:2*nxe+1
     nm += 1
     nf[1, nm] = 0
     nf[2, nm] = 0
   end
 end
end