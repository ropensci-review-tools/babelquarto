# Quick local smoke-test for render_presentation().
# Run from the package root, e.g.:
#   source("dev/test_presentation.R")
# or open in RStudio and hit Ctrl+Shift+Enter.
#
# Requirements: quarto CLI on PATH, plus pkgload (or devtools).
#
# NOTE on site_url:
#   Do NOT pass site_url = "https://example.com" to render_presentation() when
#   testing locally.  Leaving site_url = NULL (the default) causes render() to
#   set it to "" in interactive sessions, which produces root-relative hrefs
#   (e.g. /de/index.html).  These resolve correctly when served by servr below,
#   but would navigate to the literal domain if hardcoded.

pkgload::load_all()   # load babelquarto from source

# ------------------------------------------------------------------
# 1. Scaffold a two-language presentation project
# ------------------------------------------------------------------
out_dir <- fs::path(fs::path_temp(), "bq_presentation_test")
if (fs::dir_exists(out_dir)) fs::dir_delete(out_dir)
fs::dir_create(out_dir)
message("Building presentation project in: ", out_dir)

quarto_multilingual_presentation(
  parent_dir = out_dir,
  project_dir = "my_slides",
  main_language = "en",
  further_languages = "de",
  site_url = "https://example.com"   # only used in the config / sitemap
)

# ------------------------------------------------------------------
# 2. Give the German deck some distinct content so the switch is obvious
# ------------------------------------------------------------------
brio::write_lines(
  c(
    "---",
    'title: "Meine Präsentation"',
    "format: revealjs",
    "---",
    "",
    "## Folie 1",
    "",
    "Inhalt auf **Deutsch**.",
    "",
    "## Folie 2",
    "",
    "Zweite Folie."
  ),
  file.path(out_dir, "my_slides", "index.de.qmd")
)

# Give the English deck a second slide too
en_lines <- brio::read_lines(file.path(out_dir, "my_slides", "index.qmd"))
brio::write_lines(
  c(en_lines, "", "## Slide 2", "", "Second slide."),
  file.path(out_dir, "my_slides", "index.qmd")
)

# Customise pill labels
config_path <- file.path(out_dir, "my_slides", "_quarto.yml")
config_lines <- brio::read_lines(config_path)
config_lines[config_lines == '    text: "Version in de"'] <- '    text: "Deutsch"'
config_lines[config_lines == '    text: "Version in en"'] <- '    text: "English"'
brio::write_lines(config_lines, config_path)

# ------------------------------------------------------------------
# 3. Render  (site_url omitted → "" in interactive mode → root-relative links)
# ------------------------------------------------------------------
message("Rendering...")
render_presentation(
  file.path(out_dir, "my_slides"),
  preview = FALSE
)

# ------------------------------------------------------------------
# 4. Serve locally and open in browser
#    servr::httw() is needed so root-relative hrefs (/de/index.html) resolve.
# ------------------------------------------------------------------
site <- file.path(out_dir, "my_slides", "_site")
message("\n--- Output ---")
message("  English deck : ", file.path(site, "index.html"))
message("  German deck  : ", file.path(site, "de", "index.html"))
message("\nStarting local server at ", site, " ...")
message("Click the pill to switch language. Press Ctrl+C / Escape to stop.")
servr::httw(site)   # opens browser and serves; Ctrl+C to stop
