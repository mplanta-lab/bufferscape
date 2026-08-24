## Submission

This is an update of an existing CRAN package (current version on CRAN: 1.0.3).

### What has changed

* Input files may now be **GeoPackage** as well as KML. The format is detected
  from the file's contents, and a file may describe one site or several in
  either format.
* The bundled `mare_categories` dataset has been brought into line with the
  nomenclature published in the accompanying article: 28 classes in nine
  thematic groups, rather than 29 classes in twelve. This changes the class
  labels used to build output column names; it is documented in NEWS.md.
* Two fixes: a buffer polygon digitised under its site's own name was treated as
  land cover, and `bufferscape::class_dictionary()` failed unless the package
  had been attached with `library()`.

See NEWS.md for the full list.

## Test environments

* local: Ubuntu 24.04, R 4.3.3
* win-builder: R-devel and R release
* GitHub Actions: ubuntu-latest (R release, R devel), macos-latest (R release),
  windows-latest (R release)

## R CMD check results

0 errors | 0 warnings | 1 note

* The note reports possibly invalid URLs for doi.org and orcid.org. Both
  resolve in a browser; those hosts return HTTP 403 to automated requests.

## Reverse dependencies

None; no other package depends on bufferscape.
