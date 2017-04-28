using Documenter, PtFEM

makedocs(
    format = :html,
    sitename = "PtFEM",
    pages = Any[
        "Home" => "index.md",
        #"Programs" => Any[
        #    "4 Static Equilibrium/p41.md",
        #    "4 Static Equilibrium/p42.md",
        #    ],
        #"BuildingBlocks" => Any[
        #    "PtFEM/Line.md",
        #   ],
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
    deps = nothing,
    make = nothing,
)