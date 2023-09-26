
<!-- README.md is generated from README.Rmd. Please edit that file -->

# babelquarto

<!-- badges: start -->

[![R-CMD-check](https://github.com/ropensci-review-tools/quartobabel/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/ropensci-review-tools/quartobabel/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->

The goal of babelquarto is to render a Quarto multilingual project, book
or website.

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

## Example book

Create a starter/example book.

``` r
parent_dir <- withr::local_tempdir()
project_dir <- "blop"
babelquarto::quarto_multilingual_book(parent_dir = parent_dir, project_dir = project_dir)
#> [1] "/tmp/Rtmpi5n7Fb/file18787551a1364/blop"
readLines(file.path(parent_dir, project_dir, "_quarto.yml"))
#>  [1] "project:"                          "  type: book"                     
#>  [3] ""                                  "book:"                            
#>  [5] "  site-url: https://example.com"   "  title: \"blop\""                
#>  [7] "  author: \"Firstname Lastname\""  "  date: \"01/01/2023\""           
#>  [9] "  chapters:"                       "    - index.qmd"                  
#> [11] "    - intro.qmd"                   "    - summary.qmd"                
#> [13] "    - references.qmd"              ""                                 
#> [15] "bibliography: references.bib"      ""                                 
#> [17] "format:"                           "  html:"                          
#> [19] "    theme: cosmo"                  ""                                 
#> [21] "babelquarto:"                      "  languagecodes:"                 
#> [23] "  - name: es"                      "    text: \"Version in es\""      
#> [25] "  - name: fr"                      "    text: \"Version in fr\""      
#> [27] "  - name: en"                      "    text: \"Version in en\""      
#> [29] "  mainlanguage: 'en'"              "  languages: ['es', 'fr']"        
#> [31] "title-es: title in es"             "title-fr: title in fr"            
#> [33] "description-es: description in es" "description-fr: description in fr"
#> [35] "author-es: author in es"           "author-fr: author in fr"          
#> [37] "lang: en"
fs::dir_tree(file.path(parent_dir, project_dir))
#> /tmp/Rtmpi5n7Fb/file18787551a1364/blop
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
babelquarto::render_book(file.path(parent_dir, project_dir))
```

We end up with three books, that cross-link to each other from the left
sidebar. [Example](https://devdevguide.netlify.app).

## Example website

Create a starter/example website.

``` r
parent_dir <- withr::local_tempdir()
project_dir <- "blop"
babelquarto::quarto_multilingual_website(parent_dir = parent_dir, project_dir = project_dir)
#> [1] "/tmp/Rtmpi5n7Fb/file1878733fb6c8d/blop"
readLines(file.path(parent_dir, project_dir, "_quarto.yml"))
#>  [1] "project:"                          "  type: website"                  
#>  [3] ""                                  "website:"                         
#>  [5] "  site-url: https://example.com"   "  title: \"blop\""                
#>  [7] "  navbar:"                         "    left:"                        
#>  [9] "      - href: index.qmd"           "        text: Home"               
#> [11] "      - about.qmd"                 ""                                 
#> [13] "format:"                           "  html:"                          
#> [15] "    theme: cosmo"                  "    css: styles.css"              
#> [17] "    toc: true"                     ""                                 
#> [19] ""                                  ""                                 
#> [21] ""                                  "babelquarto:"                     
#> [23] "  languagecodes:"                  "  - name: es"                     
#> [25] "    text: \"Version in es\""       "  - name: fr"                     
#> [27] "    text: \"Version in fr\""       "  - name: en"                     
#> [29] "    text: \"Version in en\""       "  mainlanguage: 'en'"             
#> [31] "  languages: ['es', 'fr']"         "title-es: title in es"            
#> [33] "title-fr: title in fr"             "description-es: description in es"
#> [35] "description-fr: description in fr" "author-es: author in es"          
#> [37] "author-fr: author in fr"           "lang: en"
fs::dir_tree(file.path(parent_dir, project_dir))
#> /tmp/Rtmpi5n7Fb/file1878733fb6c8d/blop
#> ├── _quarto.yml
#> ├── about.es.qmd
#> ├── about.fr.qmd
#> ├── about.qmd
#> ├── index.es.qmd
#> ├── index.fr.qmd
#> ├── index.qmd
#> └── styles.css
```

``` r
babelquarto::render_website(file.path(parent_dir, project_dir))
```

[Example](https://maelle.github.io/babelsite),
[source](https://github.com/maelle/babelsite)

## Configure the base URL

Use the [usual Quarto
field](https://quarto.org/docs/websites/website-tools.html), or use the
`site_url` argument of `babelquarto::render_book()`.

    book:
      site-url: https://example.com

## Content translation

From a book whose main language is English…

``` r
parent_dir <- withr::local_tempdir()
project_dir <- "babelbook"
quarto_bin <- quarto::quarto_path()
withr::with_dir(parent_dir, {
  sys::exec_wait(
    quarto_bin,
    args = c("create-project", project_dir, "--type", "book")
  )
})
#> [1] 0
```

- Register languages in the Quarto configuration, for instance

``` r
project_path <- file.path(parent_dir, project_dir)
babelquarto::register_main_language(main_language = "en", project_path = project_path)
babelquarto::register_further_languages(further_languages = "es", project_path = project_path)
```

This is how the config file now looks like:

``` yaml
project:
  type: book

book:
  title: "babelbook"
  author: "Norah Jones"
  date: "9/26/2023"
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
  languagecodes:
  - name: es
    text: "Version in es"
  - name: en
    text: "Version in en"
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

Note that babelquarto does not *translate* the content! Translation
tooling lives in [babeldown](https://docs.ropensci.org/babeldown).
