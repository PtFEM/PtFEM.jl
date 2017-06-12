using Documenter, PtFEM

makedocs(
    format = :html,
    sitename = "PtFEM",
    pages = Any[
        "Introduction" => "INTRO.md",
        "Getting started" => "GETTINGSTARTED.md",
        "Changes w.r.t. PtFEM" => "CHANGES.md",
        "PtFEM.jl documentation" => "index.md",
        "Versions" => "VERSIONS.md",
        "Todo" => "TODO.md",
        "References" => "REFERENCES.md",
    ]
)

deploydocs(
    repo = "github.com/PtFEM/PtFEM.jl.git",
    target = "build",
    julia = "0.6",
    osname = "linux",
    latest = "v0.0.4",
    deps = nothing,
    make = nothing
)
