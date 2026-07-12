# EU Regions explorer — source

Source for the interactive explorer served at https://fausto.ge/eu-regions/
("Digital Capabilities in the European Regions").

## Contents
- `build_explorer.R` — R script that generates the self-contained `explorer.html`
- `data/` — the six input files the script reads:
  `region_layer_matrix.rds`, `region_averages.rds`, `region_typology.rds`,
  `current_capabilities.rds`, `potential_capabilities.rds`, `eu27_nuts2_sf.rds`

## Rebuild
1. In `build_explorer.R`, point `an_dir` (line ~26) at this folder's `data/`
   (the committed copy still points at the original working directory on G:).
2. Run the script in R. It produces `explorer.html`.
3. Deploy: copy the output over the served page and push:
   ```
   cp explorer.html ../eu-regions/index.html
   git add . && git commit -m "Update EU regions explorer" && git push
   ```

Snapshot taken 2026-07-12 from
`G:\My Drive\phd works\eurostack regions\automated versions\260507 analysis\`
(script of 2026-07-12; data of 2026-07-09). That folder remains the working
copy — re-copy script + data here when the analysis moves.
