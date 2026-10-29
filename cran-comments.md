## Submission

This is an update of an existing CRAN package (current version on CRAN:
1.0.3). It collects the changes made since that release; NEWS.md lists them in
full.

### Main changes since 1.0.3

* Input may now be GeoPackage as well as KML. The format is detected from the
  file's contents.
* The bundled `mare_categories` dataset now matches the nomenclature of the
  accompanying article (28 classes in nine thematic groups).
* Robustness for hand-digitised data: self-intersecting polygons are repaired
  instead of aborting a file, and sites or polygons digitised twice are
  detected rather than double-counted. The output workbook is always written.
* Figures: the colour palette was rebuilt so that no two classes are
  indistinguishable, the north arrow now points north, and textures are drawn
  completely on concave polygons.

## Test environments

* local: Ubuntu 24.04, R 4.3.3
* win-builder: R-devel
* GitHub Actions: ubuntu-latest (R release and R devel), macos-latest
  (R release), windows-latest (R release)

## R CMD check results

0 errors | 0 warnings | 0 notes

## Reverse dependencies

None; no other package depends on bufferscape.
