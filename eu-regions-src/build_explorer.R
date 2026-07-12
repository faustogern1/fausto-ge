###############################################################################
# build_explorer.R
#
# Generates an interactive, self-contained HTML tool for exploring the
# EU-27 NUTS2 digital-capability indices, matching the visual style of the
# R-produced maps (palettes from 06_maps.R).
#
# OUTPUT:  automated versions/260507 analysis/app/explorer.html
#          (single self-contained file, ~1-2 MB)
#
# INPUTS:  out/region_layer_matrix.rds
#          out/region_averages.rds
#          out/region_typology.rds
#          out/current_capabilities.rds  (sub-indicators incl. HGE)
#          out/potential_capabilities.rds (sub-indicators incl. SAFE)
#          out/eu27_nuts2_sf.rds
#          giscoR country shapes (year 2020, resolution 20)
#
# AUTHOR: Fausto Gernone
###############################################################################

suppressPackageStartupMessages({
  library(dplyr); library(tidyr); library(sf); library(jsonlite); library(giscoR)
})

an_dir   <- "G:/My Drive/phd works/eurostack regions/automated versions/260507 analysis/out"
out_html <- "G:/My Drive/phd works/eurostack regions/automated versions/260507 analysis/app/explorer.html"

# -----------------------------------------------------------------------------
# 1. Load and join data
# -----------------------------------------------------------------------------
rlm     <- readRDS(file.path(an_dir, "region_layer_matrix.rds"))
ra      <- readRDS(file.path(an_dir, "region_averages.rds"))
ty      <- readRDS(file.path(an_dir, "region_typology.rds"))
nuts2_sf <- readRDS(file.path(an_dir, "eu27_nuts2_sf.rds"))

# Wide per-layer table
wide <- rlm %>%
  select(nuts2, layer, current_capability, potential_capability, untapped_potential) %>%
  pivot_wider(
    names_from  = layer,
    values_from = c(current_capability, potential_capability, untapped_potential),
    names_glue  = "{.value}_{layer}"
  ) %>%
  rename_with(~ sub("_capability_", "_", .x)) %>%
  rename_with(~ sub("untapped_potential_", "untapped_", .x))

# Combine
all_data <- ra %>%
  select(nuts2, avg_current, avg_potential, avg_untapped) %>%
  left_join(ty %>% select(nuts2, typology), by = "nuts2") %>%
  left_join(wide, by = "nuts2")

# -----------------------------------------------------------------------------
# 1b. Sub-component indicators that underpin the per-layer current / potential
#     (current = mean of SBS + patents; potential = mean of relatedness +
#      graduates + CRM + reversed energy, depending on layer)
# -----------------------------------------------------------------------------
cc_sub <- readRDS(file.path(an_dir, "current_capabilities.rds")) %>%
  select(nuts2, layer, sbs = sbs_index, patent = patent_index,
         hge = hge_index)
pc_sub <- readRDS(file.path(an_dir, "potential_capabilities.rds")) %>%
  select(nuts2, layer,
         relatedness = relatedness_index, graduate = graduate_index,
         crm = crm_index, energy = energy_index, safe = safe_index)
subc_wide <- full_join(cc_sub, pc_sub, by = c("nuts2", "layer")) %>%
  pivot_wider(
    names_from  = layer,
    values_from = c(sbs, patent, hge, relatedness, graduate, crm, energy, safe),
    names_glue  = "sub_{.value}_{layer}"
  )
all_data <- all_data %>% left_join(subc_wide, by = "nuts2")

cat("Indicators per region:\n"); print(names(all_data))

# -----------------------------------------------------------------------------
# 2. Project + simplify geometry
# -----------------------------------------------------------------------------
cat("\nProjecting NUTS2 to EPSG:3035 and simplifying...\n")
nuts2_proj <- nuts2_sf %>%
  st_transform(3035) %>%
  st_simplify(preserveTopology = TRUE, dTolerance = 2500)

cat("Loading + simplifying country shapes (giscoR)...\n")
countries_sf <- gisco_get_countries(year = "2020", resolution = "20")
europe_countries <- countries_sf %>%
  st_transform(3035) %>%
  st_simplify(preserveTopology = TRUE, dTolerance = 3000)

# Cap to the BBOX the R maps use
europe_bbox <- st_bbox(c(xmin = 2200000, ymin = 1200000,
                         xmax = 6800000, ymax = 5700000),
                       crs = 3035)
europe_countries <- st_crop(europe_countries, europe_bbox)

# -----------------------------------------------------------------------------
# 3. Attach indicators to projected NUTS2
# -----------------------------------------------------------------------------
nuts2_with_data <- nuts2_proj %>%
  select(NUTS_ID, NAME_LATN, CNTR_CODE) %>%
  left_join(all_data, by = c("NUTS_ID" = "nuts2"))

# -----------------------------------------------------------------------------
# 3b. Compute medians per metric (used for above/below colouring in popup)
# -----------------------------------------------------------------------------
metric_cols <- c("avg_current","avg_potential","avg_untapped",
                 paste0("current_",   c("raw_materials","hardware","networks","software")),
                 paste0("potential_", c("raw_materials","hardware","networks","software")),
                 paste0("untapped_",  c("raw_materials","hardware","networks","software")))
medians <- lapply(metric_cols, function(m) median(all_data[[m]], na.rm = TRUE))
names(medians) <- metric_cols
medians_json <- toJSON(medians, auto_unbox = TRUE, na = "null")

# -----------------------------------------------------------------------------
# 4. Convert to GeoJSON strings via tempfile round-trip
# -----------------------------------------------------------------------------
write_geojson_str <- function(sf_obj) {
  tmpf <- tempfile(fileext = ".geojson")
  st_write(sf_obj, tmpf, driver = "GeoJSON",
           delete_dsn = TRUE, quiet = TRUE,
           layer_options = c("COORDINATE_PRECISION=0"))
  txt <- paste(readLines(tmpf, warn = FALSE), collapse = "")
  unlink(tmpf)
  txt
}

cat("\nSerialising GeoJSON...\n")
nuts2_json     <- write_geojson_str(nuts2_with_data)
countries_json <- write_geojson_str(europe_countries %>% select(CNTR_ID, NAME_ENGL))

cat("Sizes (chars):  nuts2 =", nchar(nuts2_json),
    " countries =", nchar(countries_json), "\n")

# -----------------------------------------------------------------------------
# 5. HTML template (R-map palettes + interactive logic)
# -----------------------------------------------------------------------------
html <- '<!DOCTYPE html>
<html lang="en">
<head>
<meta charset="utf-8">
<title>Digital Capabilities in the European Regions: A composite indicator of regional positioning across the digital stack</title>
<style>
  :root {
    --ink: #1a1a1a; --body: #444; --mute: #777;
    --rule: #ddd; --paper: #fff;
    --non-eu: #ebebeb; --non-eu-border: #c8c8c8;
    --country-border: #959595; --nuts-border: #ffffff;
    /* Map height: 78vh at most, tempered on shorter windows so the footer
       disclaimers stay close to the fold. 89vh - 118px is the midpoint
       between a fixed 78vh and a strict fit-everything cap (100vh - 235px). */
    --maph: min(78vh, calc(89vh - 118px));
  }
  * { box-sizing: border-box; }
  html, body { margin: 0; padding: 0; background: var(--paper); color: var(--ink);
               font-family: "Source Sans 3","Source Sans Pro","Segoe UI",Arial,sans-serif;
               -webkit-font-smoothing: antialiased; }
  header { padding: 16px 36px 0; text-align: center; position: relative; }
  header h1 { font-size: 22px; margin: 0; font-weight: 700; letter-spacing: -0.2px; }
  header .sub { font-size: 13px; color: var(--mute); margin: 3px 0 0; }
  #about-btn { position: absolute; right: 36px; top: 18px;
               font-size: 11px; font-weight: 600; letter-spacing: 0.6px;
               text-transform: uppercase; padding: 7px 12px;
               background: var(--ink); border: 1px solid var(--ink);
               cursor: pointer; color: white; font-family: inherit; }
  #about-btn:hover { background: #3a3a3a; border-color: #3a3a3a; }
  /* About / methodology modal */
  #about-overlay { position: fixed; inset: 0; background: rgba(26,26,26,0.45);
                   display: none; z-index: 300;
                   align-items: center; justify-content: center; padding: 30px; }
  #about-overlay.open { display: flex; }
  .about-card { background: white; max-width: 720px; width: 100%%;
                max-height: 86vh; border: 1px solid #c8c8c8;
                box-shadow: 0 12px 44px rgba(0,0,0,0.25);
                position: relative; text-align: left;
                display: flex; flex-direction: column; overflow: hidden; }
  .about-scroll { overflow-y: auto; scrollbar-width: thin;
                  padding: 28px 36px 26px; }
  .about-card h2 { font-size: 19px; margin: 0 34px 2px 0; font-weight: 700;
                   color: var(--ink); }
  .about-card .about-sub { font-size: 12px; color: var(--mute); margin: 0 0 8px; }
  .about-card h3 { font-size: 11px; font-weight: 700; color: var(--mute);
                   text-transform: uppercase; letter-spacing: 1px;
                   margin: 18px 0 5px; }
  .about-card p  { font-size: 13px; color: var(--body); line-height: 1.55;
                   margin: 4px 0 8px; }
  .about-card ul { margin: 2px 0 8px 18px; padding: 0; }
  .about-card li { font-size: 13px; color: var(--body); line-height: 1.5;
                   margin: 3px 0; }
  .about-card b  { color: var(--ink); }
  #about-close { position: absolute; top: 12px; right: 16px; cursor: pointer;
                 color: #aaa; font-size: 22px; line-height: 1; border: none;
                 background: none; padding: 0; }
  #about-close:hover { color: #333; }
  footer .linklike { background: none; border: none; padding: 0; margin: 0;
                     font: inherit; color: var(--body); cursor: pointer;
                     text-decoration: underline; }
  /* Side panel */
  .map-area { grid-column: 1; min-width: 0;
              display: flex; justify-content: center; }
  .side-panel { grid-column: 2; display: flex; flex-direction: column;
                gap: 12px; height: var(--maph); min-height: 0; box-sizing: border-box; }
  .side-panel h4 { font-size: 10px; font-weight: 700; color: var(--mute);
                   text-transform: uppercase; letter-spacing: 1px;
                   margin: 0 0 8px; }
  /* Light frame around the map and each side-panel widget */
  .widget { border: 1px solid #c8c8c8; background: white;
            padding: 12px 14px; box-sizing: border-box; }
  /* Square-ish frame matching the 820x800 viewBox; explicit width (derived
     from the height) so the grid auto column sizes to it deterministically. */
  .widget-map { padding: 6px; height: var(--maph); width: calc(var(--maph) * 82 / 80);
                display: flex; align-items: center; justify-content: center;
                position: relative; overflow: hidden; }
  /* Zoom controls (top-left of the map frame) */
  #zoom-controls { position: absolute; top: 10px; left: 10px; z-index: 5;
                   display: flex; flex-direction: column; gap: 4px; }
  #zoom-controls button { width: 28px; height: 28px; border: 1px solid #c8c8c8;
                          background: white; cursor: pointer; font-size: 15px;
                          line-height: 1; color: var(--ink); padding: 0;
                          font-family: inherit; }
  #zoom-controls button:hover { background: #f1efe9; }
  svg#map.zoomed { cursor: grab; }
  svg#map.panning { cursor: grabbing; }
  .widget-typ-counts { flex: 0 0 auto; }
  .widget-typ-detail, .widget-ranking {
    flex: 1 1 auto; min-height: 0;
    display: flex; flex-direction: column;
  }
  .side-panel table { width: 100%%; border-collapse: collapse; font-size: 12px; }
  .side-panel td { padding: 4px 4px; vertical-align: top;
                   border-bottom: 1px solid #eee; }
  .side-panel tr { cursor: pointer; transition: background 0.08s; }
  .side-panel tr:hover { background: #f1efe9; }
  .side-panel td.rank { color: var(--mute); width: 26px;
                        font-variant-numeric: tabular-nums; font-size: 11px; }
  .side-panel td.region { color: var(--ink); }
  .side-panel td.cc { color: var(--mute); font-size: 11px;
                      width: 28px; text-transform: uppercase; }
  .side-panel td.score { text-align: right; font-variant-numeric: tabular-nums;
                         font-weight: 600; color: var(--ink); width: 44px; }
  /* Scrollable ranking */
  .side-panel .ranking-scroll {
    flex: 1 1 auto; min-height: 0;
    overflow-y: auto;
    border-top: 1px solid #e5e5e5;
    border-bottom: 1px solid #e5e5e5;
    scrollbar-width: thin;
  }
  .side-panel .ranking-scroll thead th {
    position: sticky; top: 0; background: #ffffff;
    font-size: 9px; color: var(--mute); font-weight: 700;
    text-transform: uppercase; letter-spacing: 1px;
    padding: 7px 4px 6px; border-bottom: 1px solid #ddd;
    text-align: left; z-index: 2;
  }
  .side-panel .ranking-scroll thead th.score-h { text-align: right; }
  .side-panel .rank-count { font-size: 10px; color: var(--mute);
                            margin: 6px 0 0; }
  .side-panel .typology-list { display: flex; flex-direction: column; gap: 6px; }
  .side-panel .typ-row { display: flex; align-items: center; gap: 8px;
                         font-size: 12px; color: var(--body); }
  .side-panel .typ-row .sw { width: 12px; height: 12px; }
  .side-panel .typ-row .typ-n { margin-left: auto; font-weight: 600;
                                 color: var(--ink); font-variant-numeric: tabular-nums; }
  /* Selected-region detailed view (typology mode only) */
  .typ-panel { display: contents; }
  .detail-box { flex: 1 1 auto; display: flex; flex-direction: column;
                min-height: 0; margin-top: 2px; }
  .detail-box .detail-hint { color: var(--mute); font-size: 11px;
                             margin: 4px 0; line-height: 1.4; }
  .detail-head { flex: 0 0 auto; display: flex; align-items: center; gap: 6px;
                 font-weight: 700; font-size: 13px; color: var(--ink);
                 margin: 2px 0 0; }
  .detail-head .flag { width: 18px; height: 13px; border: 1px solid #eee;
                       border-radius: 2px; flex-shrink: 0; }
  .detail-meta { flex: 0 0 auto; color: var(--mute); font-size: 10px;
                 margin: 2px 0 6px; text-transform: uppercase;
                 letter-spacing: 0.5px; }
  .detail-scroll { flex: 1 1 auto; min-height: 0; overflow-y: auto;
                   border-top: 1px solid #e5e5e5;
                   border-bottom: 1px solid #e5e5e5; scrollbar-width: thin; }
  table.detail-tbl { width: 100%%; border-collapse: collapse; font-size: 11px; }
  table.detail-tbl td { padding: 2px 3px; border-bottom: 1px solid #f3f3f3;
                        font-variant-numeric: tabular-nums; vertical-align: top; }
  table.detail-tbl td.d-lab { color: #555; }
  table.detail-tbl td.d-val { text-align: right; font-weight: 600;
                              color: var(--ink); width: 46px; }
  table.detail-tbl td.d-val.above { color: #1b7837; }
  table.detail-tbl td.d-val.below { color: #c44e52; }
  table.detail-tbl td.d-pc  { text-align: right; color: var(--mute);
                              width: 42px; font-size: 10px; }
  table.detail-tbl tr.d-strong td { font-weight: 700;
                                     border-bottom: 1px solid #dcdcdc; }
  table.detail-tbl tr.d-strong td.d-lab { color: #1f1f1f; }
  table.detail-tbl tr.d-sec td { background: #f1efe9; color: var(--mute);
                                 font-size: 9px; font-weight: 700;
                                 text-transform: uppercase; letter-spacing: 1px;
                                 padding: 5px 3px; border-bottom: none; }
  /* 2-column grid: map + its controls on the left, legend + side panel on
     the right. The legend cell (col 2, row 1) sits directly above the side
     panel (col 2, row 2), so their right edges line up automatically. */
  /* Centred on screen so the composition is balanced on wide displays */
  .layout { display: grid; grid-template-columns: max-content 280px;
            column-gap: 18px; row-gap: 6px; align-items: start;
            justify-content: center; padding: 10px 36px 14px; }
  .controls { grid-column: 1; display: flex; gap: 28px; align-items: flex-end;
              flex-wrap: wrap; padding: 0; }
  .controls .group { display: flex; gap: 10px; align-items: center; }
  .controls label { font-size: 12px; color: var(--mute);
                    text-transform: uppercase; letter-spacing: 1px; }
  select { font-size: 14px; padding: 7px 12px; border: 1px solid #c8c8c8;
           background: white; cursor: pointer; min-width: 260px;
           font-family: inherit; color: var(--ink); }
  select:focus, input[type="text"]:focus { outline: 2px solid #4a90e2; outline-offset: 0; }
  input[type="text"]#region-search {
    font-size: 14px; padding: 7px 12px; border: 1px solid #c8c8c8;
    background: white; min-width: 280px; font-family: inherit;
    color: var(--ink);
  }
  input[type="text"]#region-search::placeholder { color: var(--faint, #b5b5b5); }
  .map-wrap { padding: 0 36px 24px; }
  svg#map { display: block; height: 100%%; width: 100%%;
            background: white; }
  .country-shape { fill: var(--non-eu); stroke: var(--non-eu-border); stroke-width: 0.5;
                   pointer-events: none; }
  .country-border { fill: none; stroke: var(--country-border); stroke-width: 0.7;
                    pointer-events: none; vector-effect: non-scaling-stroke; }
  .region-shape { stroke: var(--nuts-border); stroke-width: 0.4;
                  vector-effect: non-scaling-stroke;
                  cursor: pointer; transition: stroke-width 0.08s ease, stroke 0.08s ease; }
  .region-shape:hover { stroke: #2b2b2b; stroke-width: 1.2; }
  .region-shape.row-hover {
    stroke: #1a1a1a; stroke-width: 2.4;
    filter: drop-shadow(0 0 2px rgba(0,0,0,0.35));
  }
  .region-shape.selected {
    stroke: #ffffff;
    stroke-width: 3.5;
    filter:
      drop-shadow(0 0 0 #1a1a1a)
      drop-shadow(0 0 0 #1a1a1a)
      drop-shadow(0 0 6px rgba(0,0,0,0.75));
  }
  /* Legend — occupies grid col 2, row 1, spanning the full column width so
     it stacks flush with the side-panel widgets below. In typology mode it
     stays empty: the Regions-per-category panel is the legend there. */
  #legend { grid-column: 2; align-self: end; }
  .legend-bar { width: 100%%; height: 12px; border: 1px solid #cccccc;
                box-sizing: border-box; }
  .legend-labels { display: flex; justify-content: space-between; width: 100%%;
                   margin-top: 4px; font-size: 10px; color: var(--body);
                   font-variant-numeric: tabular-nums; }
  .legend-block { display: flex; flex-direction: column; align-items: stretch; }
  .legend-title { font-size: 10px; color: var(--mute); text-transform: uppercase;
                  letter-spacing: 1px; margin-bottom: 4px; text-align: left;
                  line-height: 1.45; }
  /* Popup */
  #popup { position: fixed; background: white; border: 1px solid #c8c8c8;
           box-shadow: 0 6px 18px rgba(0,0,0,0.10); padding: 16px 20px;
           min-width: 280px; max-width: 320px; font-size: 13px; z-index: 100;
           display: none; }
  #popup h3 { margin: 0 0 2px; font-size: 16px; font-weight: 700;
              display: flex; align-items: center; gap: 8px; }
  #popup h3 .flag { width: 22px; height: 16px; border: 1px solid #eee;
                    border-radius: 2px; flex-shrink: 0; }
  #popup .meta { color: var(--mute); font-size: 11px; margin: 0 0 10px;
                 letter-spacing: 0.5px; }
  #popup td.val.above { color: #1b7837; }
  #popup td.val.below { color: #c44e52; }
  #popup .badges { display: flex; flex-wrap: wrap; gap: 6px 8px;
                   align-items: stretch; margin: 0 0 10px; }
  #popup .typ { display: inline-flex; align-items: center; padding: 3px 10px;
                font-size: 11px; font-weight: 700; color: white;
                letter-spacing: 0.4px; text-transform: uppercase; margin: 0; }
  #popup .cap-chip { display: inline-flex; align-items: center;
                     padding: 1.5px 9px; font-size: 11px; font-weight: 700;
                     color: var(--ink); letter-spacing: 0.4px;
                     text-transform: uppercase; margin: 0;
                     border: 1.5px solid var(--ink); background: #fafafa;
                     font-variant-numeric: tabular-nums; }
  #popup .section { margin: 10px 0 4px; font-size: 10px; font-weight: 700;
                    color: var(--mute); text-transform: uppercase; letter-spacing: 1px; }
  #popup table { width: 100%%; border-collapse: collapse; }
  #popup table td { padding: 3px 0; vertical-align: top; font-size: 12.5px;
                    color: var(--body); }
  #popup td.label { color: #333; }
  #popup td.val { text-align: right; font-variant-numeric: tabular-nums;
                  font-weight: 600; color: var(--ink); }
  #popup td.val.na { font-weight: 400; color: var(--mute); }
  #popup .close { position: absolute; top: 8px; right: 12px; cursor: pointer;
                  color: #aaa; font-size: 20px; line-height: 1;
                  border: none; background: none; padding: 0; }
  #popup .close:hover { color: #333; }
  footer { padding: 8px 36px 12px; color: var(--mute); font-size: 11px;
           border-top: 1px solid var(--rule); margin-top: 4px;
           text-align: center; }
  footer a { color: var(--body); }
</style>
</head>
<body>

<header>
  <h1>Digital Capabilities in the European Regions</h1>
  <p class="sub">A composite indicator of regional positioning across the digital stack</p>
  <button id="about-btn" type="button">Method &amp; info</button>
</header>

<div class="layout">
<div class="controls">
  <div class="group">
    <label for="metric">Show</label>
    <select id="metric">
      <option value="typology" selected>Regional typology</option>
      <option value="avg_current">Average current capability</option>
      <option value="avg_potential">Average potential capability</option>
      <option value="avg_untapped">Average untapped potential</option>
    </select>
  </div>
  <div class="group">
    <label for="region-search">Find</label>
    <input type="text" id="region-search" list="region-options"
           placeholder="Region, country or NUTS code (e.g. Düsseldorf, France, DEA1)" autocomplete="off" />
    <datalist id="region-options"></datalist>
  </div>
</div>

<div id="legend" class="legend-block"></div>

<div class="map-area">
  <div class="widget widget-map">
    <div id="zoom-controls">
      <button id="zoom-in" title="Zoom in" aria-label="Zoom in">+</button>
      <button id="zoom-out" title="Zoom out" aria-label="Zoom out">&minus;</button>
      <button id="zoom-reset" title="Reset view" aria-label="Reset view">&#8962;</button>
    </div>
    <svg id="map" viewBox="0 0 820 800" preserveAspectRatio="xMidYMid meet"></svg>
  </div>
</div>

<aside class="side-panel" id="side-panel"></aside>
</div>

<div id="popup" role="dialog"></div>

<!-- About / methodology modal -->
<div id="about-overlay" role="dialog" aria-modal="true" aria-labelledby="about-title">
  <div class="about-card">
    <button id="about-close" aria-label="Close">&times;</button>
    <div class="about-scroll">
    <h2 id="about-title">About this tool</h2>
    <p class="about-sub">Digital Capabilities in the European Regions &mdash; data and methodology in brief</p>

    <p>Digital industrial policy can have the side effect of exacerbating regional
       inequalities and leaving some areas behind. For this reason, it is important
       to design policies that are place-aware and aim to share the benefits of
       growth as widely as possible.</p>
    <p>This explorer visualises the data produced for
       <i>Mapping Digital Capabilities in the European Regions: A composite
       indicator of regional positioning across the digital stack</i>, a project
       for the DG Regio of the European Commission. The project asks where in
       Europe the capacity to produce digital technologies is located, how that
       capacity differs across the layers of the digital stack, and where
       conditions for expansion are not yet matched by observed activity. It
       covers the 244 NUTS-2 regions of the EU-27.</p>

    <h3>The four layers of the digital stack</h3>
    <ul>
      <li><b>Raw materials</b> &mdash; extraction and processing of the material
          inputs of digital technologies (mining, chemicals, basic metals and
          related processing).</li>
      <li><b>Hardware</b> &mdash; manufacture of electronic components, computers,
          machinery and related equipment.</li>
      <li><b>Networks &amp; connectivity</b> &mdash; telecommunications
          infrastructure and services.</li>
      <li><b>Software, data &amp; AI</b> &mdash; software publishing, programming,
          data services and related activities.</li>
    </ul>

    <h3>Three lenses</h3>
    <ul>
      <li><b>Current capability</b> &mdash; observed digital activity: sectoral
          employment, patenting and high-growth firms.</li>
      <li><b>Potential capability</b> &mdash; enabling conditions for future
          activity: relatedness of the existing industrial mix, talent pipelines,
          energy costs, material deposits and access to finance.</li>
      <li><b>Untapped potential</b> &mdash; the conversion gap, defined as
          max(0, potential &minus; current). High values flag regions whose
          conditions outpace their observed activity.</li>
    </ul>

    <h3>The regional typology</h3>
    <p>Regions are classified in two steps. A region is an
       <b>Established leader</b> if its current capability sits in the top decile
       of at least two layers. The remaining regions are split by the medians
       of average current capability and average untapped potential:
       <b>Strong performers</b> (current at or above the median),
       <b>Latent hubs</b> (current below the median, untapped at or above it) and
       <b>Digital laggards</b> (below both medians).</p>

    <h3>How the indices are built</h3>
    <p>All indicators are log-transformed and min&ndash;max normalised to [0,1]
       across the EU-27, so scores express relative position within the EU rather
       than absolute magnitudes. For each region and layer:</p>
    <ul>
      <li><b>Current capability</b> is the average of the employment, patenting and
          high-growth-firm indicators.</li>
      <li><b>Potential capability</b> averages relatedness with layer-specific
          enablers &mdash; talent (hardware, networks, software), electricity prices
          (reversed; raw materials and hardware) and critical-raw-material deposits
          (raw materials) &mdash; with access to finance entering every layer at
          half weight.</li>
      <li><b>Untapped potential</b> = max(0, potential &minus; current).</li>
    </ul>
    <p>Averages are computed on available components: missing indicators are
       dropped rather than treated as zeros. Cross-layer averages are the mean over
       the four layers.</p>

    <h3>Data sources</h3>
    <ul>
      <li><b>Employment</b> &mdash; Eurostat Structural Business Statistics,
          regional employment in digital NACE divisions (reference year 2022).</li>
      <li><b>Patenting</b> &mdash; OECD REGPAT, patents allocated to regions by
          inventor address, priority years 2020&ndash;2024.</li>
      <li><b>High-growth firms</b> &mdash; Orbis (Bureau van Dijk) employment
          panel, Eurostat&ndash;OECD definition: at least 10 employees at the start
          of the window and at least 10%% average annualised employment growth over
          three years, latest accounts 2022 or later. Counts are adjusted for
          cross-country differences in Orbis coverage against Eurostat business
          demography.</li>
      <li><b>Talent</b> &mdash; ETER, graduates in engineering and ICT fields
          (2023 extraction).</li>
      <li><b>Electricity prices</b> &mdash; Eurostat industrial electricity prices,
          2024&ndash;2025 half-years, reversed so that lower prices score higher.
          Country level.</li>
      <li><b>Material deposits</b> &mdash; JRC data on critical raw material
          deposits.</li>
      <li><b>Access to finance</b> &mdash; EC/ECB SAFE survey (wave 36, 2025),
          share of SMEs identifying access to finance as their most important
          problem, reversed. Country level, half weight.</li>
    </ul>

    <p style="margin-top:18px; border-top: 1px solid var(--rule); padding-top: 12px;">
       &copy; Fausto Gernone for DG Regio of the European Commission. All rights
       reserved. For access to the underlying data or questions about the project,
       please contact the author.</p>
    </div>
  </div>
</div>

<footer>
  <div>Data sources: Eurostat Structural Business Statistics; OECD REGPAT; Orbis (Bureau van Dijk);
       ETER; JRC critical raw material deposits; Eurostat industrial electricity prices;
       EC/ECB SAFE survey. &nbsp;<button class="linklike" id="about-link" type="button">About
       this tool &amp; methodology</button></div>
  <div style="margin-top:6px; color: var(--ink); font-weight: 600;">&copy; Fausto Gernone
       for European Commission&rsquo;s DG Regio.</div>
  <div style="margin-top:3px;">All rights reserved. For access to the underlying data or
       questions about the project, please contact the author.</div>
</footer>

<script>
/* -----------------------------------------------------------------------
 * Data (EPSG:3035 projected metres)
 * --------------------------------------------------------------------- */
const NUTS2_DATA        = __NUTS2_JSON__;
const COUNTRIES_DATA    = __COUNTRIES_JSON__;
const MEDIANS           = __MEDIANS_JSON__;

/* Flag helper - flagcdn.com PNG flags
   (Eurostat uses EL for Greece; flag is GR. UK -> GB.) */
function flagImg(cntr) {
  if (!cntr) return "";
  const iso = (cntr === "EL") ? "gr" : (cntr === "UK") ? "gb" : cntr.toLowerCase();
  return "<img class=\\"flag\\" src=\\"https://flagcdn.com/w40/" + iso + ".png\\" alt=\\"" + cntr + "\\" />";
}

/* -----------------------------------------------------------------------
 * Palettes (identical to R 06_maps.R). Untapped uses the diverging
 * RdYlBu ramp (blue = below the EU-27 median, red = above), applied on
 * an axis fitted to the observed range with the palette midpoint pinned
 * at the median — matching the report maps. The cross-layer averages
 * also use a data-fitted axis.
 * --------------------------------------------------------------------- */
const PAL = {
  current:   ["#f7fcf0","#c7e9c0","#74c476","#238b45","#00441b"],
  potential: ["#fcfbfd","#d0d1e6","#8c96c6","#8856a7","#4a1486"],
  untapped:  ["#4575b4","#74add1","#abd9e9","#e0f3f8","#ffffbf",
              "#fee090","#fdae61","#f46d43","#d73027"]
};
const TYP_COLORS = {
  "Established leaders": "#1b7837",
  "Strong performers":        "#7fbf7b",
  "Latent hubs":     "#f1b75e",
  "Digital laggards":    "#c44e52"
};
const TYP_ORDER = ["Established leaders","Strong performers","Latent hubs","Digital laggards"];

const LAYER_LABEL = {
  raw_materials: "Raw materials",
  hardware:      "Hardware",
  networks:      "Networks",
  software:      "Software"
};

/* EU outermost regions + the African enclave cities: kept in the data and
   in all EU-27 statistics, but excluded from the ranking panel (they sit
   outside the map frame and distort the top of the rankings). */
const OVERSEAS = new Set([
  "ES63", "ES64",                              // Ceuta, Melilla
  "ES70",                                      // Canarias
  "PT20", "PT30",                              // Açores, Madeira
  "FRY1", "FRY2", "FRY3", "FRY4", "FRY5"       // French outermost regions
]);

/* Sub-components that underpin each per-layer index. Current is always
   SBS + patents + high-growth enterprises; potential is layer-specific
   with SAFE access-to-finance entering every layer at half weight
   (matches 02_current_capabilities.R / 03_potential_capabilities.R). */
const CURRENT_COMPONENTS = ["sbs", "patent", "hge"];
const POTENTIAL_COMPONENTS = {
  raw_materials: ["relatedness", "crm", "energy", "safe"],
  hardware:      ["relatedness", "graduate", "energy", "safe"],
  networks:      ["relatedness", "graduate", "safe"],
  software:      ["relatedness", "graduate", "safe"]
};
const COMP_LABEL = {
  sbs:         "Employment",
  patent:      "Patenting",
  hge:         "High-growth firms",
  relatedness: "Relatedness density",
  graduate:    "Talent",
  crm:         "CRM deposits",
  energy:      "Electricity price",
  safe:        "Access to finance"
};
const LAYER_ORDER = ["raw_materials", "hardware", "networks", "software"];
// Detailed-view box uses its own (reversed) layer order, software first.
const DETAIL_LAYER_ORDER = ["software", "networks", "hardware", "raw_materials"];

/* Percentile of a value within the EU-27 distribution of one metric,
   reported as a coarse bucket ("&gt;80%", "&lt;5%"). Sorted arrays are
   memoised because this is called once per indicator row. */
const _pctCache = {};
function pctileRank(metricKey, v) {
  if (v == null || isNaN(v)) return null;
  let vals = _pctCache[metricKey];
  if (!vals) {
    vals = NUTS2_DATA.features
      .map(f => f.properties[metricKey])
      .filter(x => x != null && !isNaN(x))
      .sort((a, b) => a - b);
    _pctCache[metricKey] = vals;
  }
  if (!vals.length) return null;
  let lo = 0, hi = vals.length;
  while (lo < hi) { const m = (lo + hi) >> 1; if (vals[m] < v) lo = m + 1; else hi = m; }
  return 100 * lo / vals.length;
}
function bucketLabel(p) {
  if (p == null) return "";
  const buckets = [95, 90, 80, 70, 60, 50, 40, 30, 20, 10, 5];
  for (const b of buckets) if (p >= b) return "&gt;" + b + "%";
  return "&lt;5%";
}

/* -----------------------------------------------------------------------
 * Projection: data are already in projected metres (EPSG:3035).
 * Fit BBOX into the SVG viewBox preserving aspect ratio.
 * --------------------------------------------------------------------- */
const BBOX = { xmin: 2400000, ymin: 1400000, xmax: 6600000, ymax: 5500000 };
const VB_W = 820, VB_H = 800, PAD = 12;
const sx = (VB_W - 2*PAD) / (BBOX.xmax - BBOX.xmin);
const sy = (VB_H - 2*PAD) / (BBOX.ymax - BBOX.ymin);
const SCALE = Math.min(sx, sy);
const OFF_X = (VB_W - (BBOX.xmax - BBOX.xmin) * SCALE) / 2;
const OFF_Y = (VB_H - (BBOX.ymax - BBOX.ymin) * SCALE) / 2;

function project(x, y) {
  return [
    OFF_X + (x - BBOX.xmin) * SCALE,
    VB_H - OFF_Y - (y - BBOX.ymin) * SCALE
  ];
}

function pathFromGeom(geom) {
  if (!geom) return "";
  const polys = geom.type === "Polygon" ? [geom.coordinates] : geom.coordinates;
  const parts = [];
  for (const poly of polys) {
    for (const ring of poly) {
      let s = "M";
      for (let i = 0; i < ring.length; i++) {
        const [px, py] = project(ring[i][0], ring[i][1]);
        s += px.toFixed(1) + "," + py.toFixed(1);
        if (i < ring.length - 1) s += "L";
      }
      s += "Z";
      parts.push(s);
    }
  }
  return parts.join("");
}

/* -----------------------------------------------------------------------
 * Color helpers — replicate the ggplot scales in 06_maps.R:
 *   current_/potential_ by layer : fixed 0-1 axis
 *   avg_current / avg_potential  : axis fitted to the observed range
 *   untapped (layer + average)   : fitted axis, diverging palette with
 *                                  its midpoint pinned at the EU-27 median
 * --------------------------------------------------------------------- */
function paletteFor(metric) {
  if (metric === "avg_current"   || metric.startsWith("current_"))   return PAL.current;
  if (metric === "avg_potential" || metric.startsWith("potential_")) return PAL.potential;
  if (metric === "avg_untapped"  || metric.startsWith("untapped_"))  return PAL.untapped;
  return null;
}
function hexToRgb(h) {
  const n = parseInt(h.slice(1), 16);
  return [(n >> 16) & 255, (n >> 8) & 255, n & 255];
}
function mix(a, b, t) {
  return [0,1,2].map(i => Math.round(a[i] + (b[i] - a[i]) * t));
}
function rgbStr(arr) {
  return "rgb(" + arr.join(",") + ")";
}
function sampleRamp(u, pal) {
  u = Math.max(0, Math.min(1, u));
  const n = pal.length - 1;
  const idx = Math.min(Math.floor(u * n), n - 1);
  const frac = u * n - idx;
  return rgbStr(mix(hexToRgb(pal[idx]), hexToRgb(pal[idx+1]), frac));
}
const _scaleCache = {};
function scaleFor(metric) {
  if (_scaleCache[metric]) return _scaleCache[metric];
  const pal = paletteFor(metric);
  if (!pal) return null;
  const isUntapped = (metric === "avg_untapped") || metric.startsWith("untapped_");
  const fitted = isUntapped || metric === "avg_current" || metric === "avg_potential";
  let d0 = 0, d1 = 1, midRel = null;
  if (fitted) {
    const vals = NUTS2_DATA.features
      .map(f => f.properties[metric])
      .filter(v => v != null && !isNaN(v));
    d0 = Math.min.apply(null, vals);
    d1 = Math.max.apply(null, vals);
    if (!(d1 > d0)) { d0 = 0; d1 = 1; }
  }
  // the diverging (untapped) palette keeps its colour midpoint pinned at
  // the EU-27 median (matches the report maps); not shown in the legend
  if (isUntapped) {
    const med = MEDIANS[metric];
    midRel = Math.min(0.98, Math.max(0.02, (med - d0) / (d1 - d0)));
  }
  return (_scaleCache[metric] = { pal, d0, d1, midRel, fitted });
}
function colorFor(v, sc) {
  if (v == null || isNaN(v)) return "var(--non-eu)";
  let t = (v - sc.d0) / (sc.d1 - sc.d0);
  t = Math.max(0, Math.min(1, t));
  const u = (sc.midRel == null)
    ? t
    : (t <= sc.midRel ? 0.5 * t / sc.midRel
                      : 0.5 + 0.5 * (t - sc.midRel) / (1 - sc.midRel));
  return sampleRamp(u, sc.pal);
}

/* -----------------------------------------------------------------------
 * Build the SVG
 * --------------------------------------------------------------------- */
const NS = "http://www.w3.org/2000/svg";
const svg = document.getElementById("map");

// Layer 1: non-EU country shapes (background)
const gBg = document.createElementNS(NS, "g");
gBg.setAttribute("id", "g-countries");
COUNTRIES_DATA.features.forEach(f => {
  const p = document.createElementNS(NS, "path");
  p.setAttribute("d", pathFromGeom(f.geometry));
  p.setAttribute("class", "country-shape");
  gBg.appendChild(p);
});
svg.appendChild(gBg);

// Layer 2: NUTS2 regions
const gReg = document.createElementNS(NS, "g");
gReg.setAttribute("id", "g-regions");
const regionPaths = {};
NUTS2_DATA.features.forEach(f => {
  const p = document.createElementNS(NS, "path");
  p.setAttribute("d", pathFromGeom(f.geometry));
  p.setAttribute("class", "region-shape");
  p.dataset.id = f.properties.NUTS_ID;
  p.addEventListener("click", (e) => {
    if (suppressClick) return;   // a drag-pan just ended
    showPopup(f, e);
  });
  gReg.appendChild(p);
  regionPaths[f.properties.NUTS_ID] = p;
});
svg.appendChild(gReg);

// Layer 3: country borders (above regions)
const gBorders = document.createElementNS(NS, "g");
gBorders.setAttribute("id", "g-borders");
COUNTRIES_DATA.features.forEach(f => {
  const p = document.createElementNS(NS, "path");
  p.setAttribute("d", pathFromGeom(f.geometry));
  p.setAttribute("class", "country-border");
  gBorders.appendChild(p);
});
svg.appendChild(gBorders);

/* -----------------------------------------------------------------------
 * Zoom & pan (viewBox-based): mouse wheel zooms about the cursor,
 * drag pans, buttons zoom about the centre. Region strokes use
 * vector-effect so they stay hairline at any zoom.
 * --------------------------------------------------------------------- */
const view = { x: 0, y: 0, w: VB_W, h: VB_H };
const MAX_ZOOM = 16;
let suppressClick = false;

function applyView() {
  svg.setAttribute("viewBox",
    view.x + " " + view.y + " " + view.w + " " + view.h);
  svg.classList.toggle("zoomed", view.w < VB_W - 0.5);
}
function clampView() {
  view.w = Math.min(VB_W, Math.max(VB_W / MAX_ZOOM, view.w));
  view.h = view.w * (VB_H / VB_W);
  view.x = Math.min(VB_W - view.w, Math.max(0, view.x));
  view.y = Math.min(VB_H - view.h, Math.max(0, view.y));
}
// Client pixel -> viewBox coordinates (accounts for preserveAspectRatio
// letterboxing inside the rendered element box).
function clientToVb(ev) {
  const r = svg.getBoundingClientRect();
  const s = Math.min(r.width / view.w, r.height / view.h);
  const padX = (r.width  - view.w * s) / 2;
  const padY = (r.height - view.h * s) / 2;
  return [ view.x + (ev.clientX - r.left - padX) / s,
           view.y + (ev.clientY - r.top  - padY) / s ];
}
function zoomAbout(vx, vy, factor) {
  const w0 = view.w;
  let w1 = Math.min(VB_W, Math.max(VB_W / MAX_ZOOM, w0 / factor));
  const k = w1 / w0;
  view.x = vx - (vx - view.x) * k;
  view.y = vy - (vy - view.y) * k;
  view.w = w1;
  clampView();
  applyView();
}
svg.addEventListener("wheel", (e) => {
  e.preventDefault();
  const [vx, vy] = clientToVb(e);
  zoomAbout(vx, vy, e.deltaY < 0 ? 1.25 : 0.8);
}, { passive: false });

let panState = null;
svg.addEventListener("pointerdown", (e) => {
  if (e.button !== 0) return;
  // Do NOT capture the pointer here. Capturing on pointerdown retargets
  // the subsequent click event to the <svg>, so a plain click would
  // never reach the region-path click handler (that broke selection).
  // We capture only once a real drag begins (in pointermove).
  panState = { id: e.pointerId, x0: e.clientX, y0: e.clientY,
               vx0: view.x, vy0: view.y, moved: false, captured: false };
});
svg.addEventListener("pointermove", (e) => {
  if (!panState) return;
  const dx = e.clientX - panState.x0, dy = e.clientY - panState.y0;
  if (!panState.moved && Math.hypot(dx, dy) < 4) return;
  if (!panState.captured) {
    try { svg.setPointerCapture(panState.id); } catch (_) {}
    panState.captured = true;
  }
  panState.moved = true;
  svg.classList.add("panning");
  const r = svg.getBoundingClientRect();
  const s = Math.min(r.width / view.w, r.height / view.h);
  view.x = panState.vx0 - dx / s;
  view.y = panState.vy0 - dy / s;
  clampView();
  applyView();
});
function endPan(e) {
  if (!panState) return;
  if (panState.captured) {
    try { svg.releasePointerCapture(panState.id); } catch (_) {}
  }
  if (panState.moved) {
    suppressClick = true;                // swallow the click after a drag
    setTimeout(() => { suppressClick = false; }, 0);
  }
  panState = null;
  svg.classList.remove("panning");
}
svg.addEventListener("pointerup", endPan);
svg.addEventListener("pointercancel", endPan);

document.getElementById("zoom-in").addEventListener("click",
  () => zoomAbout(view.x + view.w / 2, view.y + view.h / 2, 1.5));
document.getElementById("zoom-out").addEventListener("click",
  () => zoomAbout(view.x + view.w / 2, view.y + view.h / 2, 1 / 1.5));
document.getElementById("zoom-reset").addEventListener("click", () => {
  view.x = 0; view.y = 0; view.w = VB_W; view.h = VB_H;
  applyView();
});

/* -----------------------------------------------------------------------
 * Color the regions according to current metric
 * --------------------------------------------------------------------- */
let currentMetric = "typology";

function updateMap(metric) {
  const isTyp = metric === "typology";
  const sc = scaleFor(metric);
  if (isTyp) {
    // No floating popup in typology mode; hide it if it was left open.
    const pu = document.getElementById("popup");
    if (pu) pu.style.display = "none";
  }
  NUTS2_DATA.features.forEach(f => {
    const p = regionPaths[f.properties.NUTS_ID];
    if (!p) return;
    if (isTyp) {
      const t = f.properties.typology;
      p.style.fill = TYP_COLORS[t] || "var(--non-eu)";
    } else {
      p.style.fill = colorFor(f.properties[metric], sc);
    }
  });
  drawLegend(metric, sc, isTyp);
  updateSidePanel(metric, isTyp);
}

/* -----------------------------------------------------------------------
 * Side panel: top 5 / bottom 5 per metric (or typology distribution)
 * --------------------------------------------------------------------- */
function updateSidePanel(metric, isTyp) {
  const panel = document.getElementById("side-panel");

  if (isTyp) {
    // Counts per typology category
    const counts = {};
    TYP_ORDER.forEach(t => counts[t] = 0);
    NUTS2_DATA.features.forEach(f => {
      const t = f.properties.typology;
      if (t && counts[t] !== undefined) counts[t] += 1;
    });
    let countsInner = "<h4>Regions per category</h4>" +
                      "<div class=\\"typology-list\\">";
    TYP_ORDER.forEach(t => {
      countsInner += "<div class=\\"typ-row\\">" +
              "<span class=\\"sw\\" style=\\"background:" + TYP_COLORS[t] + "\\"></span>" +
              "<span>" + t + "</span>" +
              "<span class=\\"typ-n\\">" + counts[t] + "</span>" +
              "</div>";
    });
    countsInner += "</div>";
    const detailInner = "<h4>Selected region &mdash; detailed view</h4>" +
                        "<div id=\\"detail-box\\" class=\\"detail-box\\"></div>";
    panel.innerHTML =
      "<section class=\\"widget widget-typ-counts\\">" + countsInner + "</section>" +
      "<section class=\\"widget widget-typ-detail\\">" + detailInner + "</section>";
    renderDetailBox();
    return;
  }

  // Rank by selected metric, drop NAs. Overseas territories (EU outermost
  // regions) and the African enclave cities are excluded from the ranking:
  // they sit outside the map frame and their small, atypical economies
  // distort the top of the untapped ranking. They remain in the data, the
  // search and all EU-27 statistics (medians, percentiles, colour scales).
  const rows = NUTS2_DATA.features
    .map(f => ({
      id:    f.properties.NUTS_ID,
      name:  f.properties.NAME_LATN,
      cc:    f.properties.CNTR_CODE,
      v:     f.properties[metric]
    }))
    .filter(r => r.v != null && !isNaN(r.v) && !OVERSEAS.has(r.id));
  rows.sort((a, b) => b.v - a.v);
  const N = rows.length;

  const bodyRows = rows.map((r, i) =>
      "<tr data-id=\\"" + r.id + "\\">" +
      "<td class=\\"rank\\">" + (i + 1) + "</td>" +
      "<td class=\\"region\\">" + r.name + "</td>" +
      "<td class=\\"cc\\">" + r.cc + "</td>" +
      "<td class=\\"score\\">" + r.v.toFixed(3) + "</td>" +
      "</tr>"
    ).join("");

  panel.innerHTML =
    "<section class=\\"widget widget-ranking\\">" +
      "<h4>Ranking</h4>" +
      "<div class=\\"ranking-scroll\\"><table>" +
        "<thead><tr>" +
          "<th>#</th><th>Region</th><th></th><th class=\\"score-h\\">Score</th>" +
        "</tr></thead>" +
        "<tbody>" + bodyRows + "</tbody>" +
      "</table></div>" +
      "<p class=\\"rank-count\\">" + N + " regions ranked &middot; " +
        "overseas territories excluded</p>" +
    "</section>";

  // Wire up row interactions
  panel.querySelectorAll("tr[data-id]").forEach(tr => {
    const id = tr.dataset.id;
    const path = regionPaths[id];

    // Hover: highlight the matching region on the map
    tr.addEventListener("mouseenter", () => {
      if (path) path.classList.add("row-hover");
    });
    tr.addEventListener("mouseleave", () => {
      if (path) path.classList.remove("row-hover");
    });

    // Click: open popup near the cursor
    tr.addEventListener("click", (e) => {
      const f = NUTS2_DATA.features.find(x => x.properties.NUTS_ID === id);
      if (!f) return;
      showPopup(f, e);
    });
  });
}

/* -----------------------------------------------------------------------
 * Detailed-view box (shown in the side panel in typology mode only).
 * Sticky to the last clicked region (detailId), so it persists even
 * after the floating popup is dismissed.
 * --------------------------------------------------------------------- */
function detailRow(label, metricKey, v, strong) {
  const s = (v == null || isNaN(v)) ? "&mdash;" : Number(v).toFixed(3);
  const p = pctileRank(metricKey, v);
  const pc = bucketLabel(p);
  const cls = (p == null) ? "" : (p >= 50 ? " above" : " below");
  return "<tr" + (strong ? " class=\\"d-strong\\"" : "") + ">" +
         "<td class=\\"d-lab\\">" + label + "</td>" +
         "<td class=\\"d-val" + cls + "\\">" + s + "</td>" +
         "<td class=\\"d-pc\\">" + pc + "</td></tr>";
}

function renderDetailBox() {
  const box = document.getElementById("detail-box");
  if (!box) return;                       // not in typology mode
  if (!detailId) {
    box.innerHTML = "<p class=\\"detail-hint\\">Click a region on the map " +
                    "(or in the popup) to see its underlying indicators and " +
                    "their EU-27 percentiles.</p>";
    return;
  }
  const f = NUTS2_DATA.features.find(x => x.properties.NUTS_ID === detailId);
  if (!f) { box.innerHTML = ""; return; }
  const pp = f.properties;

  let h = "<div class=\\"detail-head\\">" + flagImg(pp.CNTR_CODE) +
          "<span>" + pp.NAME_LATN + "</span></div>" +
          "<p class=\\"detail-meta\\">" + pp.NUTS_ID + " &middot; " +
          (pp.typology || "&mdash;") + "</p>" +
          "<div class=\\"detail-scroll\\"><table class=\\"detail-tbl\\"><tbody>";

  h += "<tr class=\\"d-sec\\"><td colspan=\\"3\\">Cross-layer averages</td></tr>";
  h += detailRow("Current",   "avg_current",   pp.avg_current,   true);
  h += detailRow("Potential", "avg_potential", pp.avg_potential, true);
  h += detailRow("Untapped",  "avg_untapped",  pp.avg_untapped,  true);

  DETAIL_LAYER_ORDER.forEach(L => {
    h += "<tr class=\\"d-sec\\"><td colspan=\\"3\\">" + LAYER_LABEL[L] + "</td></tr>";
    // Order: Current -> its raw indicators -> Potential -> its raw
    //        indicators -> Untapped
    h += detailRow("Current", "current_" + L, pp["current_" + L], true);
    CURRENT_COMPONENTS.forEach(c => {
      const k = "sub_" + c + "_" + L;
      h += detailRow(COMP_LABEL[c], k, pp[k], false);
    });
    h += detailRow("Potential", "potential_" + L, pp["potential_" + L], true);
    (POTENTIAL_COMPONENTS[L] || []).forEach(c => {
      const k = "sub_" + c + "_" + L;
      h += detailRow(COMP_LABEL[c], k, pp[k], false);
    });
    h += detailRow("Untapped",  "untapped_"  + L, pp["untapped_"  + L], true);
  });

  h += "</tbody></table></div>";
  box.innerHTML = h;
}

function drawLegend(metric, sc, isTyp) {
  const el = document.getElementById("legend");
  // Typology mode: no top legend — the Regions-per-category panel below
  // (swatches + labels + counts) is the legend.
  if (isTyp) { el.innerHTML = ""; return; }
  const titleMap = {
    avg_current:   "Current capability index",
    avg_potential: "Potential capability index",
    avg_untapped:  "Untapped potential index"
  };
  const title = titleMap[metric] || metric;
  el.innerHTML = "<div class=\\"legend-title\\">" + title + "</div>";
  {
    const fmt2 = v => Number(v).toFixed(2).replace(/0$/, "").replace(/\\.$/, "");
    // Gradient stops honour the pinned midpoint so the bar shows the
    // same non-linear mapping the map uses.
    const n = sc.pal.length - 1;
    const stopPos = i => {
      const u = i / n;
      if (sc.midRel == null) return u;
      return (u <= 0.5)
        ? sc.midRel * (u / 0.5)
        : sc.midRel + (1 - sc.midRel) * ((u - 0.5) / 0.5);
    };
    const stops = sc.pal.map((c, i) =>
      c + " " + (100 * stopPos(i)).toFixed(1) + "%").join(",");
    const bar = document.createElement("div");
    bar.className = "legend-bar";
    bar.style.background = "linear-gradient(to right, " + stops + ")";
    el.appendChild(bar);
    const lbls = document.createElement("div");
    lbls.className = "legend-labels";
    if (!sc.fitted) {
      lbls.innerHTML = "<span>0</span><span>0.25</span><span>0.50</span><span>0.75</span><span>1</span>";
    } else {
      lbls.innerHTML = "<span>" + fmt2(sc.d0) + "</span>" +
        "<span>" + fmt2(sc.d1) + "</span>";
    }
    el.appendChild(lbls);
  }
}

/* -----------------------------------------------------------------------
 * Popup
 * --------------------------------------------------------------------- */
let selectedId = null;
let detailId   = null;   // sticky selection for the side-panel detail box

function fmt(v) {
  if (v == null || isNaN(v)) return null;
  return Number(v).toFixed(3);
}

function row(label, v, metricKey) {
  const s = fmt(v);
  let extra = "";
  if (s == null) {
    extra = " na";
  } else if (metricKey && MEDIANS[metricKey] != null) {
    const med = MEDIANS[metricKey];
    if (v > med) extra = " above";
    else if (v < med) extra = " below";
  }
  return "<tr><td class=\\"label\\">" + label + "</td>" +
         "<td class=\\"val" + extra + "\\">" +
         (s == null ? "&mdash;" : s) + "</td></tr>";
}

function showPopup(feature, ev) {
  ev.stopPropagation();
  if (selectedId && regionPaths[selectedId]) {
    regionPaths[selectedId].classList.remove("selected");
  }
  selectedId = feature.properties.NUTS_ID;
  detailId   = feature.properties.NUTS_ID;
  const selPath = regionPaths[selectedId];
  selPath.classList.add("selected");
  // Bring to front of its group so the thick stroke is not clipped
  // by neighbouring region paths drawn later.
  selPath.parentNode.appendChild(selPath);
  // Refresh sticky side-panel box
  renderDetailBox();

  // In typology mode the sticky side-panel box IS the viewer — no popup.
  if (currentMetric === "typology") return;

  const pp = feature.properties;
  const typ = pp.typology || "\\u2014";
  const typBg = TYP_COLORS[typ] || "#888";
  const popup = document.getElementById("popup");

  // Popup content is specific to the capability shown on the map:
  // the average level sits next to the typology badge, followed by the
  // layer decomposition of that capability only.
  const family = currentMetric.substring(4);        // current|potential|untapped
  const FAMILY_TITLE = { current: "Current capability",
                         potential: "Potential capability",
                         untapped: "Untapped potential" };
  const FAMILY_SHORT = { current: "Current",
                         potential: "Potential",
                         untapped: "Untapped" };
  const famLabel = FAMILY_TITLE[family] || family;
  const avgVal = pp["avg_" + family];

  popup.innerHTML =
    "<button class=\\"close\\" aria-label=\\"Close\\">&times;</button>" +
    "<h3>" + flagImg(pp.CNTR_CODE) + "<span>" + pp.NAME_LATN + "</span></h3>" +
    "<p class=\\"meta\\">" + pp.NUTS_ID + " &middot; " + pp.CNTR_CODE + "</p>" +
    "<div class=\\"badges\\">" +
    "<span class=\\"typ\\" style=\\"background:" + typBg + "\\">" + typ + "</span>" +
    "<span class=\\"cap-chip\\">" + FAMILY_SHORT[family] + ": " +
      (fmt(avgVal) == null ? "&mdash;" : fmt(avgVal)) + "</span></div>" +
    "<div class=\\"section\\">" + famLabel + " by layer</div>" +
    "<table>" +
      row("Raw materials", pp[family + "_raw_materials"], family + "_raw_materials") +
      row("Hardware",      pp[family + "_hardware"],      family + "_hardware") +
      row("Networks",      pp[family + "_networks"],      family + "_networks") +
      row("Software",      pp[family + "_software"],      family + "_software") +
    "</table>";

  popup.querySelector(".close").addEventListener("click", closePopup);

  // Position near click, keep inside viewport (measured size, not guessed)
  popup.style.display = "block";
  const rect = popup.getBoundingClientRect();
  let x = ev.clientX + 14;
  let y = ev.clientY + 14;
  if (x + rect.width + 8 > window.innerWidth)  x = ev.clientX - rect.width - 14;
  if (y + rect.height + 8 > window.innerHeight) y = Math.max(8, window.innerHeight - rect.height - 12);
  popup.style.left = Math.max(8, x) + "px";
  popup.style.top  = Math.max(8, y) + "px";
}

function closePopup() {
  document.getElementById("popup").style.display = "none";
  if (selectedId && regionPaths[selectedId]) {
    regionPaths[selectedId].classList.remove("selected");
  }
  selectedId = null;
}

document.addEventListener("click", (e) => {
  if (suppressClick) return;   // a drag-pan just ended
  const popup = document.getElementById("popup");
  if (popup.style.display !== "block") return;
  if (popup.contains(e.target)) return;
  if (e.target.classList && e.target.classList.contains("region-shape")) return;
  closePopup();
});
document.addEventListener("keydown", (e) => {
  if (e.key === "Escape") {
    const ov = document.getElementById("about-overlay");
    if (ov && ov.classList.contains("open")) { closeAbout(); return; }
    closePopup();
  }
});

/* -----------------------------------------------------------------------
 * About / methodology modal
 * --------------------------------------------------------------------- */
function openAbout()  {
  document.getElementById("about-overlay").classList.add("open");
  const sc = document.querySelector(".about-scroll");
  if (sc) sc.scrollTop = 0;          // always reopen at the top
}
function closeAbout() { document.getElementById("about-overlay").classList.remove("open"); }
document.getElementById("about-btn").addEventListener("click", openAbout);
document.getElementById("about-link").addEventListener("click", openAbout);
document.getElementById("about-close").addEventListener("click", closeAbout);
document.getElementById("about-overlay").addEventListener("click", (e) => {
  if (e.target === document.getElementById("about-overlay")) closeAbout();
});
if ((location.hash || "") === "#about") openAbout();   // deep link

/* -----------------------------------------------------------------------
 * Region search (region name, country name or NUTS code, autocomplete)
 * --------------------------------------------------------------------- */
const searchInput = document.getElementById("region-search");
const datalist    = document.getElementById("region-options");

/* EU-27 country names (Eurostat codes: EL = Greece). Typing a country
   name filters the autocomplete to that country and Enter jumps to its
   first region. */
const COUNTRY_NAME = {
  AT: "Austria",  BE: "Belgium",  BG: "Bulgaria", HR: "Croatia",
  CY: "Cyprus",   CZ: "Czechia",  DK: "Denmark",  EE: "Estonia",
  FI: "Finland",  FR: "France",   DE: "Germany",  EL: "Greece",
  HU: "Hungary",  IE: "Ireland",  IT: "Italy",    LV: "Latvia",
  LT: "Lithuania", LU: "Luxembourg", MT: "Malta", NL: "Netherlands",
  PL: "Poland",   PT: "Portugal", RO: "Romania",  SK: "Slovakia",
  SI: "Slovenia", ES: "Spain",    SE: "Sweden"
};
const COUNTRY_ALIAS = { "czech republic": "CZ", "holland": "NL",
                        "hellas": "EL", "deutschland": "DE" };

// Autocomplete: value stays "Region (CODE)"; the label carries the
// country name so the dropdown also matches country-name queries.
NUTS2_DATA.features
  .slice()
  .sort((a, b) => (a.properties.NAME_LATN || "").localeCompare(b.properties.NAME_LATN || ""))
  .forEach(f => {
    const opt = document.createElement("option");
    opt.value = f.properties.NAME_LATN + " (" + f.properties.NUTS_ID + ")";
    opt.label = COUNTRY_NAME[f.properties.CNTR_CODE] || f.properties.CNTR_CODE;
    datalist.appendChild(opt);
  });

function countryCodeFor(q) {
  if (COUNTRY_ALIAS[q]) return COUNTRY_ALIAS[q];
  for (const code in COUNTRY_NAME) {
    if (COUNTRY_NAME[code].toLowerCase() === q) return code;
  }
  for (const code in COUNTRY_NAME) {
    if (COUNTRY_NAME[code].toLowerCase().startsWith(q) && q.length >= 3) return code;
  }
  return null;
}

function findRegion(query) {
  if (!query) return null;
  const q = query.trim().toLowerCase();
  if (!q) return null;
  // 1. Exact NUTS_ID
  let hit = NUTS2_DATA.features.find(f =>
    (f.properties.NUTS_ID || "").toLowerCase() === q);
  if (hit) return hit;
  // 2. Exact "Name (CODE)"
  hit = NUTS2_DATA.features.find(f =>
    ((f.properties.NAME_LATN || "") + " (" + f.properties.NUTS_ID + ")").toLowerCase() === q);
  if (hit) return hit;
  // 3. Exact NAME_LATN
  hit = NUTS2_DATA.features.find(f =>
    (f.properties.NAME_LATN || "").toLowerCase() === q);
  if (hit) return hit;
  // 4. Country name -> first region of that country (alphabetical)
  const cc = countryCodeFor(q);
  if (cc) {
    const of = NUTS2_DATA.features
      .filter(f => f.properties.CNTR_CODE === cc)
      .sort((a, b) => (a.properties.NAME_LATN || "")
        .localeCompare(b.properties.NAME_LATN || ""));
    if (of.length) return of[0];
  }
  // 5. Substring on name or code
  hit = NUTS2_DATA.features.find(f =>
    (f.properties.NAME_LATN || "").toLowerCase().includes(q) ||
    (f.properties.NUTS_ID || "").toLowerCase().includes(q));
  return hit || null;
}

function runSearch() {
  const f = findRegion(searchInput.value);
  if (!f) return;
  // Briefly flash the region by ramping stroke
  const p = regionPaths[f.properties.NUTS_ID];
  if (p) {
    p.classList.add("selected");
    // Centroid position for the popup (in screen coords)
    let cx = 0, cy = 0, n = 0;
    const geom = f.geometry;
    const polys = geom.type === "Polygon" ? [geom.coordinates] : geom.coordinates;
    for (const poly of polys) {
      for (const ring of poly) {
        for (const [x, y] of ring) { cx += x; cy += y; n++; }
      }
    }
    if (n > 0) {
      cx /= n; cy /= n;
      const [px, py] = project(cx, cy);
      // viewBox -> screen, honouring the current zoom view and the
      // preserveAspectRatio letterboxing
      const rect = svg.getBoundingClientRect();
      const s = Math.min(rect.width / view.w, rect.height / view.h);
      const padX = (rect.width  - view.w * s) / 2;
      const padY = (rect.height - view.h * s) / 2;
      const screenX = rect.left + padX + (px - view.x) * s;
      const screenY = rect.top  + padY + (py - view.y) * s;
      const fakeEv = {
        clientX: screenX, clientY: screenY,
        stopPropagation: () => {}
      };
      showPopup(f, fakeEv);
      return;
    }
  }
  // Fallback positioning
  showPopup(f, { clientX: window.innerWidth/2, clientY: 150, stopPropagation: () => {} });
}

searchInput.addEventListener("change", runSearch);
searchInput.addEventListener("keydown", (e) => {
  if (e.key === "Enter") { e.preventDefault(); runSearch(); }
});

/* -----------------------------------------------------------------------
 * Wire up dropdown + initial render. The selected metric is mirrored in
 * the URL hash so views are shareable (e.g. explorer.html#avg_untapped).
 * --------------------------------------------------------------------- */
const metricSel = document.getElementById("metric");
metricSel.addEventListener("change", (e) => {
  currentMetric = e.target.value;
  history.replaceState(null, "", "#" + currentMetric);
  updateMap(currentMetric);
});

const initHash = decodeURIComponent((location.hash || "").replace(/^#/, ""));
if (initHash &&
    Array.prototype.some.call(metricSel.options, o => o.value === initHash)) {
  metricSel.value = initHash;
  currentMetric = initHash;
}
updateMap(currentMetric);
</script>
</body>
</html>
'

# Restore literal % from the %% used during the (abandoned) sprintf escaping
html <- gsub("%%", "%", html, fixed = TRUE)
# Inject the GeoJSON blobs
html <- sub("__NUTS2_JSON__",     nuts2_json,     html, fixed = TRUE)
html <- sub("__COUNTRIES_JSON__", countries_json, html, fixed = TRUE)
html <- sub("__MEDIANS_JSON__",   medians_json,   html, fixed = TRUE)

# -----------------------------------------------------------------------------
# 6. Write the file
# -----------------------------------------------------------------------------
writeLines(html, out_html, useBytes = TRUE)

cat("\nWritten:\n  ", out_html, "\n",
    "Size: ", round(file.info(out_html)$size / 1024, 1), " KB\n", sep = "")
