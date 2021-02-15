# std::regex has a bug on non-UTF-8 MBCS locale. We use Japanese locale for
# testing, but it's not available on all machines (GitHub Actions' Windows
# runner has it installed, though).
skip_if_japanese_locale_unavailable <- function() {
  loc <- Sys.getlocale("LC_COLLATE")
  on.exit(Sys.setlocale("LC_COLLATE", loc))

  tryCatch(
    Sys.setlocale("LC_COLLATE", "Japanese"),
    warning = function(e) {
      skip("Japanese locale is not available")
    }
  )
}

test_that("parse_lat() doesn't hang on Japanese locale", {
  skip_if_japanese_locale_unavailable()
  skip_if_not_installed("callr")
  skip_if_not_installed("pkgbuild")
  skip_if_not_installed("pkgload")

  # Since parse_lat("10") might take very long time if it hits the std::regex's
  # bug, so we need to call it as an external process so that we can set timeout.
  test_func <- function() {
    pkgload::load_all(".", recompile = FALSE)
    withr::with_locale(
      c(LC_COLLATE = "Japanese"),
      {
        loc <- Sys.getlocale("LC_COLLATE")
        parse_lat("10")
        # Check if the locale stays the same.
        if(!identical(Sys.getlocale("LC_COLLATE"), loc)) {
          stop("locale changed after executing parse_lat()", call. = FALSE)
        }
      }
    )
  }

  expect_error(callr::r_safe(test_func, timeout = 10), NA)
})
