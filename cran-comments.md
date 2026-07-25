## Test environments

* Ubuntu 24.04, R 4.3.3 (local)

## R CMD check results

0 errors | 0 warnings | 1 note

The note reports that the suggested package 'maptiles' was unavailable in the
check environment. It is used only to fetch optional satellite basemap tiles in
`map_basemap()`, is guarded by `requireNamespace()`, and its absence
degrades to a plain background with a warning rather than an error.

## Notes for the reviewer

* This is a new submission.
* Examples that draw figures or download map tiles are wrapped in `\donttest{}`.
* No functions write to the user's filespace unless a path is supplied
  explicitly; the examples use `tempfile()`/`tempdir()`.
