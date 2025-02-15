## Release summary
This is the first resubmission of `deprivateR` to CRAN as part of our initial release. All reviewer feedback has been addressed, including:

* The title has been adjusted to remove the words "Tools for" as requested
* The description has been updated to spell out these acronyms: ADI, SVI, and NDI
* A `@return` tag has been added to the `dep_get_index` function's documentation

## Test environments

* local macOS install: R 4.4.2
* Linux ubuntu distribution (via GitHub Actions): R-devel, R-release, R-oldrel-1
* macOS (via GitHub Actions): R-release
* windows (via GitHub Actions): R-release
* winbuilder: R-release, R-oldrel, R-devel

## R CMD check results

There were no ERRORs, WARNINGs, or NOTEs with local or CI checks. There is one NOTE on winbuilder:

```
0 errors | 0 warnings | 1 note

Maintainer: 'Christopher Prener <Christopher.Prener@pfizer.com>'

New submission

Possibly misspelled words in DESCRIPTION:
  ADI (11:55)
  Gini (12:77)
  NDI (11:69)
  SVI (11:60)
  quantiles (14:5)
```

* This is a new release, and all of these words are spelled correctly
