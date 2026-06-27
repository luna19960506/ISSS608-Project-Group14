# Inside the Glass Office — Project layout

```
glass_office/
├── app.R                 # SHARED shell: data loading, sidebar, filtered_data() contract, dispatch
├── R/
│   ├── mod_network.R     # Module 1 — OWNER: Li Xinyue
│   ├── mod_temporal.R    # Module 2 — OWNER: Cheng Yuanyuan
│   └── mod_internal.R    # Module 3 — OWNER: Yang Yang
└── data/
    └── clean/            # put the .rds tables from data_prep.R here
        ├── communications.rds
        ├── round_profile.rds
        ├── rounds_env.rds
        ├── env_events_long.rds
        ├── reply_edges_agent.rds
        ├── recipient_edges.rds
        ├── network_nodes.rds
        ├── tokens_internal.rds
        ├── sentiment_by_round.rds
        └── agents.rds
```

## Rules
- Each person edits ONLY their own `R/mod_*.R`.
- `app.R` is shared — announce in the group chat before changing it; pick one owner for it.
- Do NOT change the contract: `filtered_data()` returns the filtered
  communications master table, and every module keeps the signature
  `mod_*_server(id, filtered_data, opts)`. Changing it needs all three to agree.
- Join across tables on `round_hour`, not `round_idx`.
- The `data/clean/*.rds` schema is append-only.

## Git
- Branches: `feat/network`, `feat/temporal`, `feat/internal`.
- Touch only your module file; merge back to main for integration review.

## Run
Open `app.R` from the project root (with `data/clean/` beneath it) → Run App.
The three tabs ship as runnable placeholders, so the app launches before any
module is filled in. Replace the `TODO` block in your module file with the real
visualisations.
