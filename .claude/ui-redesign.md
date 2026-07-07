# UI Redesign (branch `ui-redesign`)

Modernisation of the WISE-APP front end from Bootstrap 3 / `navbarPage` to
**Bootstrap 5 via bslib**, themed with a World Bank **`_brand.yml`**.
No backend/analysis code was touched — all server logic, reactives, input IDs,
and the dynamic `appendTab()` output-tab pattern are unchanged.

## What changed

### Theme & branding
- **`inst/app/_brand.yml`** (new): World Bank palette (navy `#002244`,
  blue `#0071BC`, cyan `#009FDA`, green/yellow/red accents) and Open Sans
  (Google font, cached locally by sass at first build). Consumed by
  `bslib::bs_theme(version = 5, brand = app_sys("app/_brand.yml"))`.
- **`inst/app/www/custom.css`** (new): all custom styling, auto-included by
  `golem::bundle_resources()`. Key classes:
  - `.hero-panel`, `.step-card`, `.connect-card` — Overview landing page
  - `.step-badge` — numbered circles in accordion/step cards
  - `.step-question` — the guiding question atop each step page
  - `.empty-state` — dashed placeholder block on Overview tabs
  - `.config-flyout` — floating weather-config panel (see below)
  - `.well` — restyles legacy `wellPanel()`s (unstyled in BS5) as bordered cards
  - `.nav-tabs …` — underline-style output tabs
  - `details/summary` — styles Step 2's native "advanced settings" collapsible

### Layout (R/)
- **`app_ui.R`**: `navbarPage` → `bslib::page_navbar` with dark navy navbar,
  nav icons, version badge, right-aligned Docs link. Added shiny's built-in
  **busy indicators** (`useBusyIndicators()` + `busyIndicatorOptions()`),
  replacing `waiter::autoWaiter`.
- **`mod_1_modelling.R` / `mod_2_simulation.R` / `mod_3_scenario.R`**:
  `fluidPage + sidebarLayout` → `bslib::layout_sidebar(sidebar = sidebar(width = 360, …))`.
  `bsplus::bs_accordion` (BS3-only, was the main migration blocker) →
  `bslib::accordion(multiple = FALSE)` with numbered badges (Step 1) and
  FontAwesome icons (Step 3). The `tabsetPanel(id = "stepN_output_tabs")` ids
  are **preserved** — the server-side `appendTab()` / `updateTabsetPanel()`
  machinery works unchanged.
- **`mod_0_overview.R`** (UI function only): landing page rebuilt as
  hero panel + three step cards (`layout_column_wrap`) + a "Data" connection
  card. The `.auto_connect()` (Posit Connect) branch mirrors the same design.
  All input/output IDs unchanged.
- **`mod_1_04_weather.R`**: per-variable **"Configure" options now open in a
  floating flyout** (`.config-flyout`) beside the sidebar instead of expanding
  inline — options are visible without scrolling. Implementation detail: the
  same `actionButton` toggle + `conditionalPanel` (odd/even click count) is
  kept, so the content stays in the DOM and **input defaults register
  immediately** (a popover/modal would have delayed input registration and
  changed model behaviour). A header with a close (×) button re-clicks the
  toggle via JS. Collapses to inline flow below 1200px viewport width.

### Loading / error states
- Built-in busy indicators (shiny ≥ 1.9 + bslib): spinners on recalculating
  outputs, navbar pulse during busy — replaces `waiter`.
- `custom.css` hides raw output errors:
  `.shiny-output-error { visibility: hidden; }` while keeping
  `validate()`/`need()` messages visible and styled. Intermediate errors
  (e.g. before data is connected) no longer flash red text.
- Each step's Overview tab now has an `.empty-state` block explaining what to
  do and where outputs will appear.

### Dependencies
- **Removed** from `Imports`: `bsplus`, `waiter` (no longer referenced).
- **Added**: `brand.yml` (required by `bs_theme(brand=)`); installed via
  `renv::install("brand.yml")` and snapshotted (note: `renv.lock` is
  gitignored in this repo).
- `NAMESPACE` regenerated with `devtools::document()`.

## Not changed (intentionally)
- All `fct_*.R` files, `app_server.R`, every module server function.
- Server-rendered UI inside leaf modules (mod_1_01…mod_3_09) — restyled
  globally via CSS (`.well`, sidebar label sizing) rather than edited.
- The non-namespaced `#results_section` `insertUI` target in
  `mod_2_02_results.R` and the selectize `.lock()` JS in `mod_1_06_model.R`.
- `inst/app/www/welcome_message.md` still exists but is no longer included
  (its content now lives in the Overview hero, in R).

## Known considerations
- **BS3 → BS5 shift**: leaf modules that used BS5 utility classes
  (`me-1`, `d-block`, `alert-light` in `mod_3_01_sp.R`) were inert under BS3
  and now render properly.
- Google font download happens once at theme compile and is cached
  (`~/Library/Caches/org.R-project.R/R/sass`); on an offline server bslib
  falls back to system fonts.
- `.config-flyout` uses `position: fixed; left: 400px` (sidebar is 360px).
  If the sidebar width changes, update both.
- `mod_3_06_policy_sim`'s `uiOutput("sim_status_ui")` has no server renderer
  (pre-existing dead placeholder; left as-is).

## Verified
- `devtools::document()` clean; full UI tree builds
  (`app_ui(NULL)` + `htmltools::renderTags`).
- App boots headless (`run_app()` on port 8123); screenshots of Overview,
  Step 1, and Step 3 confirm theme, accordions, empty states, and that no
  error text appears pre-connection.
