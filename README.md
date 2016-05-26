# remotefile

[![Project Status: WIP - Initial development is in progress, but there has not yet been a stable, usable release suitable for the public.](http://www.repostatus.org/badges/latest/wip.svg)](http://www.repostatus.org/#wip)

Access files remotely, with caching.

# Installation

```r
devtools::install_github("richfitz/remotefile")
```

# Usage

This is designed for use with the [enron corpus](https://gitlab.com/rsheets/enron_corpus) and I'll bundle the usage into a package at some point.  But this should work for now:

```r
path <- "https://gitlab.com/rsheets/enron_corpus/raw/master/sheets"
dest <- tempfile()
dir.create(dest)

x <- remotefile_init(dest, path)
head(remotefile_list(x))
head(remotefile_list(x, local=TRUE))

res <- remotefile_fetch(remotefile_list(x)[[555]], x)
file.exists(res)
```
