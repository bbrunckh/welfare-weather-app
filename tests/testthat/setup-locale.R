# Some expectations compare rendered markup with non-ASCII text (for example the
# multiplication sign). Under the C locale as.character() on htmltools tags
# escapes those characters ("<U+00D7>"), so run the suite in a UTF-8 character
# locale whatever the host default is.
if (!isTRUE(l10n_info()[["UTF-8"]])) {
  old_ctype <- Sys.getlocale("LC_CTYPE")
  for (loc in c("C.UTF-8", "en_US.UTF-8")) {
    if (nzchar(suppressWarnings(Sys.setlocale("LC_CTYPE", loc)))) break
  }
  withr::defer(Sys.setlocale("LC_CTYPE", old_ctype), teardown_env())
}
