## Test environments

* local: Ubuntu 24.04, R 4.3.3
* win-builder: R-devel
* GitHub Actions: ubuntu-latest (R release, R devel), macos-latest (R release),
  windows-latest (R release)

## R CMD check results

0 errors | 0 warnings | 1 note

* This is a new release.

* The note reports:

      Found the following (possibly) invalid URLs:
        URL: https://doi.org/10.5281/zenodo.21577714
        URL: https://orcid.org/0009-0000-3353-5368

  Both resolve correctly in a browser. doi.org and orcid.org return HTTP 403 to
  automated requests, which is why the check cannot verify them.

## Notes for the reviewer

* Examples that draw figures or download map tiles are wrapped in `\donttest{}`.
* No function writes to the user's filespace unless a path is supplied
  explicitly; examples and tests use `tempfile()` / `tempdir()`.
* Progress output uses `message()`, so it can be silenced with
  `suppressMessages()`.
* All packages in Suggests are used conditionally, guarded by
  `requireNamespace()`.
