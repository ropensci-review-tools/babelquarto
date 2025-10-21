# babelquarto (development version)

## Features

- In interactive sessions, the `render_` functions now override `site-url` to be `""` so that previewing with `servr::httw()` works.
- For easier styling, the language button now has the class `"babelquarto-languages-button"`.

## Documentation improvements

- clarify the default value of `project_path` in `render_book()`, in the "Get started" vignette.
- strengthen the case for the package in the README.
- mention `site-url` param of `quarto_multilingual_` functions.
- new section in the Get started vignette, "publishing your project".
- clarify `languagecodes` in configuration vignette.
- clarify `site-url` in configuration vignette.

## Bug fixes

- make it possible to `register_further_languages()` for more languages in a second step.
- The `render_` functions now correctly use `site_url` when provided.

# babelquarto 0.0.1

* Initial rOpenSci submission.
* Fix handling on sitemap: it needed to be conditional on there being a sitemap.
