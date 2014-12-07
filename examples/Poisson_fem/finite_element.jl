# finite_element.jl
#
# Functions related to finite element formulation.
# Uses linear triangle elements with 1 pt quadrature.
# Depends on mesh.jl
#
# Amuthan Arunkumar Ramabathiran
# (aparyap@gmail.com)
#
# Distributed under The Code Project Open License (CPOL)
# http://www.codeproject.com/info/cpol10.aspx

require("mesh.jl")

# Shape Functions
# Linear Triangle
N1(xi,eta) = 1 - xi - eta
N2(xi,eta) = xi
N3(xi,eta) = eta

dN1_xi(xi,eta)  = -1.0
dN1_eta(xi,eta) = -1.0

dN2_xi(xi,eta)  = 1.0
dN2_eta(xi,eta) = 0.0

dN3_xi(xi,eta)  = 0.0
dN3_eta(xi,eta) = 1.0

# jacobian and shape function derivatives
function get_derivs(mesh, iE, xi, eta, dn)
  n1 = mesh.topology[iE,1]
  n2 = mesh.topology[iE,2]
  n3 = mesh.topology[iE,3]

  x1 = mesh.x_node[n1,1]
  y1 = mesh.x_node[n1,2]

  x2 = mesh.x_node[n2,1]
  y2 = mesh.x_node[n2,2]

  x3 = mesh.x_node[n3,1]
  y3 = mesh.x_node[n3,2]

  x = x1*N1(xi,eta) + x2*N2(xi,eta) + x3*N3(xi,eta)
  y = y1*N1(xi,eta) + y2*N2(xi,eta) + y3*N3(xi,eta)

  area_x_2 = (x1-x3)*(y2-y1) - (x1-x2)*(y3-y1)

  dxi_x = (y3-y1)/area_x_2
  dxi_y = -(x3-x1)/area_x_2

  deta_x = -(y2-y1)/area_x_2
  deta_y = (x2-x1)/area_x_2
  
  j11 = x1*dN1_xi(xi,eta)  + x2*dN2_xi(xi,eta)  + x3*dN3_xi(xi,eta)
  j12 = x1*dN1_eta(xi,eta) + x2*dN2_eta(xi,eta) + x3*dN3_eta(xi,eta)
  j21 = y1*dN1_xi(xi,eta)  + y2*dN2_xi(xi,eta)  + y3*dN3_xi(xi,eta)
  j22 = y1*dN1_eta(xi,eta) + y2*dN2_eta(xi,eta) + y3*dN3_eta(xi,eta)
  
  jcb = abs(j11*j22 - j12*j21)

  dn[1,1] = dN1_xi(xi,eta)*dxi_x + dN1_eta(xi,eta)*deta_x
  dn[1,2] = dN1_xi(xi,eta)*dxi_y + dN1_eta(xi,eta)*deta_y      

  dn[2,1] = dN2_xi(xi,eta)*dxi_x + dN2_eta(xi,eta)*deta_x
  dn[2,2] = dN2_xi(xi,eta)*dxi_y + dN2_eta(xi,eta)*deta_y
  
  dn[3,1] = dN3_xi(xi,eta)*dxi_x + dN3_eta(xi,eta)*deta_x
  dn[3,2] = dN3_xi(xi,eta)*dxi_y + dN3_eta(xi,eta)*deta_y

  return jcb, x, y
end

# Quadrature rules
const nQuad = 1

const qx = [1/3]
const qy = [1/3]

const qw = [1/2]

# Finite element space
type fe_space_C0
  node_val    :: Array{Float64,1}
  global_id   :: Array{Int,1}
  n_dirichlet :: Int
  n_nd_int    :: Int

  function fe_space_C0(mesh::Mesh, bc_fn::Function)
    n_nd = mesh.n_nd

    node_val  = zeros(n_nd)
    global_id = zeros(Int,n_nd)

    n_db_count = 0
    n_gl_id    = 0

    for i = 1:n_nd
      x = mesh.x_node[i,1]
      y = mesh.x_node[i,2]
      if on_boundary(x,y)
        n_db_count += 1
        node_val[i] = bc_fn(x,y)
      else
        n_gl_id += 1
        global_id[i] = n_gl_id
      end
    end

    n_int = n_nd - n_db_count

    new(node_val, global_id, n_db_count, n_int)
  end
end

function write_to_file(mesh::Mesh, u::fe_space_C0)
  fu = open("u.dat","w")
  for i = 1:mesh.n_nd
    println(fu, mesh.x_node[i,1], "\t", mesh.x_node[i,2], "\t", u.node_val[i])
  end
  close(fu)
end

# calculate stiffness matrix and load vector. Methods 1 and 2
function fe_matrices(mesh, u, stiffness, fext, K, F)
  n_size = u.n_nd_int

  e_nd = 3

  x = 0.0
  y = 0.0
  
  Nx  = zeros(3)
  dNx = zeros(3,2)
  Jx  = 0.0

  ke  = zeros(e_nd,e_nd)
  fe  = zeros(e_nd)

  sparse_count = 0

  for iE = 1:mesh.n_el
    fill!(ke,0.0)
    fill!(fe,0.0)

    for iQ = 1:nQuad
      xi  = qx[iQ]
      eta = qy[iQ]
    
      Nx[1] = N1(xi,eta)
      Nx[2] = N2(xi,eta)
      Nx[3] = N3(xi,eta)

      Jx,x,y = get_derivs(mesh,iE,xi,eta,dNx)
    
      for i = 1:e_nd 
      fe[i] += qw[iQ]*Jx*fext(x,y)*Nx[i]
        for j = 1:e_nd
          ke[i,j] += qw[iQ]*Jx*stiffness(Nx,dNx,i,j)
        end
      end
    end

    for i = 1:e_nd
      ni = mesh.topology[iE,i]
      ig = u.global_id[ni]
      if ig > 0
        F[ig] += fe[i]  
        for j = 1:e_nd
          nj = mesh.topology[iE,j]
          jg = u.global_id[nj]
          if jg > 0
            sparse_count += 1
            K[ig,jg] += ke[i,j]
          else
            F[ig] -= ke[i,j]*u.node_val[nj]
          end
        end
      end
    end
  end
end

# calculate stiffness matrix and load vector. Method 3
function fe_matrices(mesh, u, stiffness, fext, F)
  n_size = u.n_nd_int

  e_nd = 3

  x = 0.0
  y = 0.0
  
  iu = zeros(Int,20*n_size)
  ju = zeros(Int,20*n_size)
  vu = zeros(20*n_size)

  Nx  = zeros(3)
  dNx = zeros(3,2)
  Jx  = 0.0

  ke  = zeros(e_nd,e_nd)
  fe  = zeros(e_nd)

  sparse_count = 0

  for iE = 1:mesh.n_el
    fill!(ke,0.0)
    fill!(fe,0.0)

    for iQ = 1:nQuad
      xi  = qx[iQ]
      eta = qy[iQ]
    
      Nx[1] = N1(xi,eta)
      Nx[2] = N2(xi,eta)
      Nx[3] = N3(xi,eta)

      Jx,x,y = get_derivs(mesh,iE,xi,eta,dNx)
    
      for i = 1:e_nd 
        fe[i] += qw[iQ]*Jx*fext(x,y)*Nx[i]
        for j = 1:e_nd
          ke[i,j] += qw[iQ]*Jx*stiffness(Nx,dNx,i,j)
        end
      end
    end

    for i = 1:e_nd
      ni = mesh.topology[iE,i]
      ig = u.global_id[ni]
      if ig > 0
        F[ig] += fe[i]  
        for j = 1:e_nd
          nj = mesh.topology[iE,j]
          jg = u.global_id[nj]
          if jg > 0
            sparse_count += 1
            try 
              iu[sparse_count] = ig
              ju[sparse_count] = jg
              vu[sparse_count] = ke[i,j]
            catch BoundsError()
              iu = vcat(iu,ig)
              ju = vcat(ju,jg)
              vu = vcat(vu,ke[i,j])
            end
          else
            F[ig] -= ke[i,j]*u.node_val[nj]
          end
        end
      end
    end
  end

  iu = iu[1:sparse_count]
  ju = ju[1:sparse_count]
  vu = vu[1:sparse_count]

  return iu,ju,vu
end

# function to solve the finite element equations
function solve_fe(mesh, u, stiffness, fext)
  n_size = u.n_nd_int
  F = zeros(n_size)

  # Method 1
  #K = zeros(n_size,n_size)
  #fe_matrices(mesh,u,stiffness,fext,K,F)
  
  # Method 2
  #K = spzeros(n_size,n_size)
  #fe_matrices(mesh,u,stiffness,fext,K,F)

  # Method 3
  iu,ju,vu = fe_matrices(mesh,u,stiffness,fext,F)
  K = sparse(iu,ju,vu,n_size,n_size)
  
  U = zeros(n_size)
  U = K\F

  for i = 1:mesh.n_nd
    ig = u.global_id[i]  
      if ig > 0
      u.node_val[i] = U[ig]
    end
  end
end
