# Setup loaded nodes entry
function update_equivalent_m_and_f!(data::Dict)
  ns = Int[]
  if :loaded_elements in keys(data)
    for t in data[:loaded_nodes]
      push!(ns, t[1])
    end
    for t in data[:loaded_elements]
      if t[1] in ns
        indx = findall(in(t[1]), ns)[1]
        data[:loaded_nodes][indx][2][1] += -2.8
        data[:loaded_nodes][indx][2][2] += -0.8
        t2 = t[1] + 1
        if !(t2 in ns)
          push!(ns, t2)
          push!(data[:loaded_nodes], (t2, [0.0  0.0]))
        end
        indx = findall(in(t2), ns)[1]
        data[:loaded_nodes][indx][2][1] += -1.2
        data[:loaded_nodes][indx][2][2] += 0.5333
      else
        force = -6.0
        moment = 3.0
        if !(t[1] in ns)
          push!(data[:loaded_nodes], (t[1], [force -moment]))
          push!(ns, t[1])
        else
          indx = findall(in(t[1]), ns)[1]
          data[:loaded_nodes][indx][2][1] += -force
          data[:loaded_nodes][indx][2][2] += -moment
        end
        t2 = t[1] + 1
        if !(t2 in ns)
          push!(data[:loaded_nodes], (t2, [force  moment]))
          push!(ns, t2)
        else
          indx = findall(in(t2), ns)[1]
          data[:loaded_nodes][indx][2][1] += force
          data[:loaded_nodes][indx][2][2] += moment
        end
      end
    end
  end
end

update_equivalent_m_and_f!(data::Dict)

data[:loaded_nodes]
