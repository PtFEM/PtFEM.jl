using Documenter, PtFEM

makedocs(
    format = :html,
    sitename = "PtFEM",
    pages = Any[
        "Introduction" => "INTRO.md",
        "Getting started" => "GETTINGSTARTED.md",
        "Examples" => "EXAMPLES.md"
        "Futures" => "FUTURES.md",
        "Changes" => "CHANGES.md",
        "Versions" => "VERSIONS.md",
        "Todo" => "TODO.md",
        "References" => "REFERENCES.md",
        "PtFEM.jl programmer's documentation" => "index.md",
    ]
)

deploydocs(
    repo = "github.com/PtFEM/PtFEM.jl.git",
    target = "build",
    julia = "0.5",
    osname = "linux",
    deps = nothing,
    make = nothing
)