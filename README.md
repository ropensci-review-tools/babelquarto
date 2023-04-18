
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
parent_dir <- withr::local_tempdir()
babelquarto::quarto_multilingual_book(parent_dir = parent_dir, book_dir = "blop")
fs::dir_tree(file.path(parent_dir, "blop"))
#> /tmp/RtmpEx5EcX/file1035f761adad/blop
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
babelquarto::render_book(book_path = file.path(parent_dir, "blop"))
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
fs::dir_tree(file.path(parent_dir, "blop", "_book"))
#> /tmp/RtmpEx5EcX/file1035f761adad/blop/_book
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
#   servr::httw(file.path(parent_dir, "blop", "_book"))
# }
```

Note that this does not *translate* the content! Translation tooling
will live in [babeldown](https://docs.ropensci.org/babeldown).

### Content translation

From a book whose main language is English…

- qmd/Rmd files. `bla.qmd` translation in Spanish would live in
  `bla.es.qmd`.
- parts. The part title translation can be stored in `_quarto.yml` like
  so:

``` yml
  - part: Building Your Package
    part-es: Construyendo tu paquete
    chapters:
    - pkg_building.Rmd
    - pkg_ci.Rmd
    - pkg_security.Rmd
```

If it does not exist, babelquarto falls back to the part title in the
main language.

- title, author, description. Their translation can be stored in
  `_quarto.yml` like so (NOT in the `book` list):

``` yml
book:
  title: Cool book
  author: Myself

title-es: Libro genial
author-es: Yo misma
```

If these fields do not exist, babelquarto falls back to their text in
the main language.
