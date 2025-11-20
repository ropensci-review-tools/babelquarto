# babelquarto 0.1.0

## Features

- In interactive sessions, the `render_` functions now override `site-url` to be `""` so that previewing with `servr::httw()` works.
- For easier styling, the language button now has the class `"babelquarto-languages-button"`.
- Add verbosity to `register_` functions, that one can turn off through an option.

## Documentation improvements

- clarify the default value of `project_path` in `render_book()`, in the "Get started" vignette.
- clarify the default value of `project_path` in `register_` functions, in the "Converting an existing project" vignette.
- strengthen the case for the package in the README.
- mention `site-url` param of `quarto_multilingual_` functions.
- new section in the Get started vignette, "publishing your project".
- clarify `languagecodes` in configuration vignette.
- clarify `site-url` in configuration vignette.
- document how to get no icon, in configuration vignette.
- clarify profile naming convention, in configuration vignette.
- clarify `BABELQUARTO_CI_URL`, in render-with-ci vignette.

## Bug fixes

- make it possible to `register_further_languages()` for more languages in a second step.
- The `render_` functions now correctly use `site_url` when provided.
- avoid unnecessary empty lines in Quarto config when using `register_main_language()`.

# babelquarto 0.0.1

* Initial rOpenSci submission.
* Fix handling on sitemap: it needed to be conditional on there being a sitemap.
