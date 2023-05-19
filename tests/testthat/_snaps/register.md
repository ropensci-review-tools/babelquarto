# register_main_language() works

    Code
      register_main_language("fr", book_path)
    Error <rlang_error>
      Can't register fr as main language.
      Main language registered as en.

---

    Code
      register_main_language("en", book_path)
    Message <cliMessage>
      i Main language already registered.

