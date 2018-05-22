using StaticArrays
using BenchmarkTools

const  ⊗ = kron

function cost(𝛉::AbstractArray, 𝒞::Tuple{AbstractArray, Vararg{AbstractArray}}, 𝒟::Tuple{AbstractArray, Vararg{AbstractArray}})
    ℳ, ℳʹ = collect(𝒟)
    Λ₁, Λ₂ = collect(𝒞)
    Jₐₘₗ = 0.0
    N = length(𝒟[1])
    𝚲ₙ = @MMatrix zeros(4,4)
    # 𝐞₁ = @SVector [1.0, 0.0, 0.0]
    # 𝐞₂ = @SVector [0.0, 1.0, 0.0]
    𝐞₁ = @SMatrix [1.0; 0.0; 0.0]
    𝐞₂ = @SMatrix [0.0; 1.0; 0.0]
    @inbounds for n = 1:N
        index = SVector(1, 2)
        𝚲ₙ[1:2,1:2] .= Λ₁[n][index, index]
        𝚲ₙ[3:4,3:4] .= Λ₂[n][index, index]
        𝐦 = ℳ[n]
        𝐦ʹ= ℳʹ[n]
        𝐔ₙ = (𝐦 ⊗ 𝐦ʹ)
        ∂ₓ𝐮ₙ =  [(𝐞₁ ⊗ 𝐦ʹ) (𝐞₂ ⊗ 𝐦ʹ) (𝐦 ⊗ 𝐞₁) (𝐦 ⊗ 𝐞₂)]
        𝐁ₙ =  ∂ₓ𝐮ₙ * 𝚲ₙ * ∂ₓ𝐮ₙ'
        𝚺ₙ = 𝛉' * 𝐁ₙ * 𝛉
        Jₐₘₗ += 𝛉' * 𝐔ₙ * (𝚺ₙ \ 𝐔ₙ') * 𝛉
    end
    Jₐₘₗ
end

# Some sample data
N = 3376821
ℳ = [@MMatrix(rand(3,1)) for i = 1:N]
ℳʹ = [@MMatrix(rand(3,1)) for i = 1:N]
Λ₁ =  [SMatrix{3,3}(diagm([1.0,1.0,0.0])) for i = 1:length(ℳ)]
Λ₂ =  [SMatrix{3,3}(diagm([1.0,1.0,0.0])) for i = 1:length(ℳ)]
t = @MVector rand(9)
𝒞 = (Λ₁,Λ₂)
𝒟 = (ℳ, ℳʹ)

cost(t,𝒞 , 𝒟)
@time cost(t,𝒞 , 𝒟)
@btime cost($t,$𝒞 , $𝒟)
