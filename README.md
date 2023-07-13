
<!-- README.md is generated from README.Rmd. Please edit that file -->

# babelquarto

<!-- badges: start -->

[![R-CMD-check](https://github.com/ropensci-review-tools/quartobabel/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/ropensci-review-tools/quartobabel/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->

The goal of babelquarto is to render a Quarto multilingual book
structured like the [rOpenSci dev
guide](https://devdevguide.netlify.app/):

- each qmd is present once for the main language,
- and once more for each other language with an extension à la `.es.qmd`

## Installation

You can install the development version of babelquarto from rOpenSci
R-universe:

``` r
install.packages('babelquarto', repos = c('https://ropensci.r-universe.dev', 'https://cloud.r-project.org'))
```

Or from [GitHub](https://github.com/) with:

``` r
# install.packages("pak")
pak::pak("ropensci-review-tools/babelquarto")
```

## Example

Create a starter/example book.

``` r
parent_dir <- withr::local_tempdir()
book_dir <- "blop"
babelquarto::quarto_multilingual_book(parent_dir = parent_dir, book_dir = book_dir)
readLines(file.path(parent_dir, book_dir, "_quarto.yml"))
#>  [1] "project:"                          "  type: book"                     
#>  [3] ""                                  "book:"                            
#>  [5] "  title: \"blop\""                 "  author: \"Maëlle Salmon\""      
#>  [7] "  date: \"7/13/2023\""             "  chapters:"                      
#>  [9] "    - index.qmd"                   "    - intro.qmd"                  
#> [11] "    - summary.qmd"                 "    - references.qmd"             
#> [13] ""                                  "bibliography: references.bib"     
#> [15] ""                                  "format:"                          
#> [17] "  html:"                           "    theme: cosmo"                 
#> [19] ""                                  "babelquarto:"                     
#> [21] "  mainlanguage: 'en'"              "  languages: ['es', 'fr']"        
#> [23] "title-es: title in es"             "title-fr: title in fr"            
#> [25] "description-es: description in es" "description-fr: description in fr"
#> [27] "author-es: author in es"           "author-fr: author in fr"          
#> [29] "lang: en"
fs::dir_tree(file.path(parent_dir, book_dir))
#> /tmp/Rtmp5thWaD/file52e23e5b1314/blop
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

``` r
babelquarto::render_book(file.path(parent_dir, book_dir))
```

We end up with three books, that cross-link to each other from the left
sidebar. [Example](https://devdevguide.netlify.app).

Note that this does not *translate* the content! Translation tooling
will live in [babeldown](https://docs.ropensci.org/babeldown).

### Content translation

From a book whose main language is English…

``` r
parent_dir <- withr::local_tempdir()
book_dir <- "babelbook"
quarto_bin <- quarto::quarto_path()
withr::with_dir(parent_dir, {
  sys::exec_wait(
    quarto_bin,
    args = c("create-project", book_dir, "--type", "book")
  )
})
#> [1] 0
```

- Register languages in the Quarto configuration, for instance

``` r
book_path <- file.path(parent_dir, book_dir)
babelquarto::register_main_language(main_language = "en", book_path = book_path)
babelquarto::register_further_languages(further_languages = "es", book_path = book_path)
```

This is how the config file now looks like:

``` yaml
project:
  type: book

book:
  title: "babelbook"
  author: "Jane Doe"
  date: "7/13/2023"
  chapters:
    - index.qmd
    - intro.qmd
    - summary.qmd
    - references.qmd

bibliography: references.bib

format:
  html:
    theme: cosmo
  pdf:
    documentclass: scrreprt




babelquarto:
  mainlanguage: 'en'
  languages: ['es']
title-es: title in es
description-es: description in es
author-es: author in es
lang: en
```

- qmd/Rmd files. `bla.qmd` translation in Spanish would live in
  `bla.es.qmd`. See [babeldown](https://docs.ropensci.org/babeldown) for
  getting an automated translation.
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
