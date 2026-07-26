## Resubmission

This is a resubmission. In response to the incoming pre-test:

* Fixed the "no visible binding for global variable" note. One of the names,
  `basemap_ready`, was a genuine bug: a helper had been lost in a refactor while
  its call site remained, so `batch_composition()` failed whenever a basemap was
  requested. The helper is restored and a regression test added. The remaining
  names are non-standard-evaluation column references used by dplyr and ggplot2,
  and are now declared with `utils::globalVariables()`.

* Reworded the DESCRIPTION to remove the two words flagged as possibly
  misspelled ("rasterisation", "greenspace").

## Test environments

* local: Ubuntu 24.04, R 4.3.3
* win-builder: R-devel and R release
* CRAN incoming pre-test: Debian (R-devel), Windows (R-devel)
* GitHub Actions: ubuntu-latest (R release, R devel), macos-latest (R release),
  windows-latest (R release)

## R CMD check results

0 errors | 0 warnings | 1 note

* This is a new submission.

## Notes for the reviewer

* Examples that draw figures or download map tiles are wrapped in `\donttest{}`.
* No function writes to the user's filespace unless a path is supplied
  explicitly; examples and tests use `tempfile()` / `tempdir()`.
* Progress output uses `message()`, so it can be silenced with
  `suppressMessages()`.
* All packages in Suggests are used conditionally, guarded by
  `requireNamespace()`.
