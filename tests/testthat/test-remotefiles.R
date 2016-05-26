context("remotefiles")

test_that("basic", {
  ## This is the path we need:
  path <- "https://gitlab.com/rsheets/enron_corpus/raw/master/sheets"
  dest <- tempfile()
  dir.create(dest)

  x <- remotefile_init(dest, path)
  head(remotefile_list(x))
  head(remotefile_list(x, local=TRUE))

  res <- remotefile_fetch(remotefile_list(x)[[555]], x)
  expect_true(file.exists(res))
})
