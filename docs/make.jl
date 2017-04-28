using Documenter, PtFEM

makedocs(
    format = :html,
    sitename = "PtFEM",
    pages = Any[
        "PtFEM.jl documentation" => "index.md",
        "FUTURES" => "FUTURES.md",
        "CHANGES" => "CHANGES.md",
        "VERSIONS" => "VERSIONS.md",
        "TODO" => "TODO.md",
        "REFERENCES" => "REFERENCES.md",
    ]
)

deploydocs(
    repo = "github.com/PtFEM/PtFEM.jl.git",
    target = "build",
    julia = "0.5",
    osname = "linux"
)