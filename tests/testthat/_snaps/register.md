# register_main_language() works

    Code
      register_main_language("fr", book_path)
    Condition
      Error in `register_main_language()`:
      ! Can't register fr as main language.
      Main language registered as en.

---

    Code
      register_main_language("en", book_path)
    Message
      i Main language already registered.

# register_further_languages() works

    Code
      register_further_languages(c("es", "fr"), book_path)
    Condition
      Error in `register_further_languages()`:
      ! Can't register further languages before a main language
      i Call `register_main_language()` first.

---

    Code
      register_main_language("en", book_path)
    Message
      v Added configuration for en to 'config_path'.

---

    Code
      register_further_languages(c("es", "fr"), book_path)
    Message
      v Added configuration for usees, fr to 'config_path'.

---

    Code
      register_further_languages(c("it"), book_path)
    Message
      v Added configuration for useit to 'config_path'.

---

    Code
      register_further_languages(c("es", "fr"), book_path)
    Message
      i All languages already registered.

