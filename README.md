
# babelquarto

<!-- badges: start -->

[![R-CMD-check](https://github.com/ropensci-review-tools/quartobabel/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/ropensci-review-tools/quartobabel/actions/workflows/R-CMD-check.yaml)
[![Codecov test coverage](https://codecov.io/gh/ropensci-review-tools/babelquarto/graph/badge.svg)](https://app.codecov.io/gh/ropensci-review-tools/babelquarto)
[![Status at rOpenSci Software Peer Review](https://badges.ropensci.org/720_status.svg)](https://github.com/ropensci/software-review/issues/720)
<!-- badges: end -->

The goal of {babelquarto} is to render a Quarto multilingual project, book
or website.

Note that babelquarto does not *translate* the content! Translation
tooling lives in {[babeldown](https://docs.ropensci.org/babeldown)}.

## Installation

You can install the development version of {babelquarto} from rOpenSci
R-universe:

``` r
install.packages('babelquarto', repos = c('https://ropensci.r-universe.dev', 'https://cloud.r-project.org'))
```

Or from [GitHub](https://github.com/) with:

``` r
# install.packages("pak")
pak::pak("ropensci-review-tools/babelquarto")
```

## Getting Started

The {babelquarto} package allows you to create and render a multilingual Quarto project, book or website.
A multilingual project is based on a main language and can feature any number of additional languages.
The languages are registered once and are then present in your `_quarto.yml` configuration file under the `babelquarto` key.
Each Quarto Markdown file in your project can then be translated into these further languages and these will be used to generate the project in each language.

If you start from scratch, you might want to look at `babelquarto::quarto_multilingual_book()` or `babelquarto::quarto_multilingual_website()` and read `vignette("babelquarto")`.

If you already have and existing Quarto project and want to convert it to a multilingual project, you can use `babelquarto::register_main_language()` and `babelquarto::register_further_languages()` to get started. For more information you can read `vignette("convert")`.

## Linking

If you use cross-references in your Quarto book, make sure they use explicit anchors, not the section title, as that title will be different between languages.

## Examples

To get a feel of what a multilingual book can look like, you can have a look at this book: [*rOpenSci Packages: Development, Maintenance, and Peer Review*](https://devguide.ropensci.org/).

For a multilingual website, you can check out [Joel Nitta's website](https://www.joelnitta.com/).
