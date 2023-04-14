
<!-- README.md is generated from README.Rmd. Please edit that file -->

# babelquarto

<!-- badges: start -->

[![R-CMD-check](https://github.com/ropensci-review-tools/quartobabel/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/ropensci-review-tools/quartobabel/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->

The goal of babelquarto is to render a Quarto multilingual book
structured like the rOpenSci dev guide:

- each qmd is present once for the main language,
- and once more for each other language with an extension à la `.es.qmd`

## Installation

You can install the development version of babelquarto from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("ropensci-review-tools/quartobabel")
```

## Example

Create a starter/example book.

``` r
parent_folder <- withr::local_tempdir()
babelquarto::quarto_multilingual_book(parent_folder = parent_folder, book_folder = "blop")
fs::dir_tree(file.path(parent_folder, "blop"))
#> /tmp/Rtmp72p5yx/filecd0e6b05b776/blop
#> ├── _quarto.yml
#> ├── cover.png
#> ├── index.es.qmd
#> ├── index.fr.qmd
#> ├── index.qmd
#> ├── intro.es.qmd
#> ├── intro.fr.qmd
#> ├── intro.qmd
#> ├── references.bib
#> ├── references.es.qmd
#> ├── references.fr.qmd
#> ├── references.qmd
#> ├── summary.es.qmd
#> ├── summary.fr.qmd
#> └── summary.qmd
```

Render it. We end up with three books, that cross-link to each other
from the left sidebar.

``` r
babelquarto::render_book(file.path(parent_folder, "blop"))
#> [1m[34m[1/4] index.qmd[39m[22m
#> [1m[34m[2/4] intro.qmd[39m[22m
#> [1m[34m[3/4] summary.qmd[39m[22m
#> [1m[34m[4/4] references.qmd[39m[22m
#> 
#> Output created: _book/index.html
#> 
#> [1m[34m[1/4] index.es.qmd[39m[22m
#> [1m[34m[2/4] intro.es.qmd[39m[22m
#> [1m[34m[3/4] summary.es.qmd[39m[22m
#> [1m[34m[4/4] references.es.qmd[39m[22m
#> 
#> Output created: _book/index.es.html
#> 
#> [1m[34m[1/4] index.fr.qmd[39m[22m
#> [1m[34m[2/4] intro.fr.qmd[39m[22m
#> [1m[34m[3/4] summary.fr.qmd[39m[22m
#> [1m[34m[4/4] references.fr.qmd[39m[22m
#> 
#> Output created: _book/index.fr.html
fs::dir_tree(file.path(parent_folder, "blop", "_book"))
#> /tmp/Rtmp72p5yx/filecd0e6b05b776/blop/_book
#> ├── es
#> │   ├── index.es.html
#> │   ├── index.html
#> │   ├── intro.es.html
#> │   ├── references.es.html
#> │   ├── search.json
#> │   ├── site_libs
#> │   │   ├── bootstrap
#> │   │   │   ├── bootstrap-icons.css
#> │   │   │   ├── bootstrap-icons.woff
#> │   │   │   ├── bootstrap.min.css
#> │   │   │   └── bootstrap.min.js
#> │   │   ├── clipboard
#> │   │   │   └── clipboard.min.js
#> │   │   ├── quarto-html
#> │   │   │   ├── anchor.min.js
#> │   │   │   ├── popper.min.js
#> │   │   │   ├── quarto-syntax-highlighting.css
#> │   │   │   ├── quarto.js
#> │   │   │   ├── tippy.css
#> │   │   │   └── tippy.umd.min.js
#> │   │   ├── quarto-nav
#> │   │   │   ├── headroom.min.js
#> │   │   │   └── quarto-nav.js
#> │   │   └── quarto-search
#> │   │       ├── autocomplete.umd.js
#> │   │       ├── fuse.min.js
#> │   │       └── quarto-search.js
#> │   └── summary.es.html
#> ├── fr
#> │   ├── index.fr.html
#> │   ├── index.html
#> │   ├── intro.fr.html
#> │   ├── references.fr.html
#> │   ├── search.json
#> │   ├── site_libs
#> │   │   ├── bootstrap
#> │   │   │   ├── bootstrap-icons.css
#> │   │   │   ├── bootstrap-icons.woff
#> │   │   │   ├── bootstrap.min.css
#> │   │   │   └── bootstrap.min.js
#> │   │   ├── clipboard
#> │   │   │   └── clipboard.min.js
#> │   │   ├── quarto-html
#> │   │   │   ├── anchor.min.js
#> │   │   │   ├── popper.min.js
#> │   │   │   ├── quarto-syntax-highlighting.css
#> │   │   │   ├── quarto.js
#> │   │   │   ├── tippy.css
#> │   │   │   └── tippy.umd.min.js
#> │   │   ├── quarto-nav
#> │   │   │   ├── headroom.min.js
#> │   │   │   └── quarto-nav.js
#> │   │   └── quarto-search
#> │   │       ├── autocomplete.umd.js
#> │   │       ├── fuse.min.js
#> │   │       └── quarto-search.js
#> │   └── summary.fr.html
#> ├── index.html
#> ├── intro.html
#> ├── references.html
#> ├── search.json
#> ├── site_libs
#> │   ├── bootstrap
#> │   │   ├── bootstrap-icons.css
#> │   │   ├── bootstrap-icons.woff
#> │   │   ├── bootstrap.min.css
#> │   │   └── bootstrap.min.js
#> │   ├── clipboard
#> │   │   └── clipboard.min.js
#> │   ├── quarto-html
#> │   │   ├── anchor.min.js
#> │   │   ├── popper.min.js
#> │   │   ├── quarto-syntax-highlighting.css
#> │   │   ├── quarto.js
#> │   │   ├── tippy.css
#> │   │   └── tippy.umd.min.js
#> │   ├── quarto-nav
#> │   │   ├── headroom.min.js
#> │   │   └── quarto-nav.js
#> │   └── quarto-search
#> │       ├── autocomplete.umd.js
#> │       ├── fuse.min.js
#> │       └── quarto-search.js
#> └── summary.html
# if (require("servr") && rlang::is_interactive()) {
#   servr::httw(file.path(parent_folder, "blop", "_book"))
# }
```

Note that this does not *translate* the content! Translation tooling
will live in [babeldown](https://docs.ropensci.org/babeldown).
