# render_website() fails when missing sidebar and languagelinks is set to sidebar

    Code
      withr::with_dir(parent_dir, render_website(project_dir))
    Output
      [1m[34m[1/2] index.qmd[39m[22m
      [1m[34m[2/2] about.qmd[39m[22m
      
      Output created: _site/index.html
      
      [1m[34m[1/2] index.qmd[39m[22m
      [1m[34m[2/2] about.qmd[39m[22m
      
      Output created: _site/index.html
      
      [1m[34m[1/2] index.qmd[39m[22m
      [1m[34m[2/2] about.qmd[39m[22m
      
      Output created: _site/index.html
      
    Condition
      Error in `map()`:
      i In index: 1.
      Caused by error in `map()`:
      i In index: 1.
      Caused by error in `.f()`:
      ! Can't find website/sidebar in _quarto.yml. You set the babelquarto/languagelinks to sidebar but also don't have a sidebar in your website.

# render_book() fails when missing navbar and languagelinks is set to navbar

    Code
      withr::with_dir(parent_dir, render_book(project_dir))
    Output
      [1m[34m[1/4] index.qmd[39m[22m
      [1m[34m[2/4] intro.qmd[39m[22m
      [1m[34m[3/4] summary.qmd[39m[22m
      [1m[34m[4/4] references.qmd[39m[22m
      
      Output created: _book/index.html
      
      [1m[34m[1/4] index.es.qmd[39m[22m
      [1m[34m[2/4] intro.es.qmd[39m[22m
      [1m[34m[3/4] summary.es.qmd[39m[22m
      [1m[34m[4/4] references.es.qmd[39m[22m
      
      Output created: _book/index.es.html
      
      [1m[34m[1/4] index.fr.qmd[39m[22m
      [1m[34m[2/4] intro.fr.qmd[39m[22m
      [1m[34m[3/4] summary.fr.qmd[39m[22m
      [1m[34m[4/4] references.fr.qmd[39m[22m
      
      Output created: _book/index.fr.html
      
    Condition
      Error in `map()`:
      i In index: 1.
      Caused by error in `map()`:
      i In index: 1.
      Caused by error in `.f()`:
      ! Can't find book/navbar in _quarto.yml. You set the babelquarto/languagelinks to navbar but also don't have a navbar in your book.

