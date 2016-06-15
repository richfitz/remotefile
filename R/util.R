is_directory <- function(path) {
  file.exists(path) && file.info(path, extra_cols=FALSE)$isdir
}

is_remote <- function(path) {
  grepl("^([a-z]+)://", path)
}
fix_windows_paths <- function(path) {
  gsub("\\", "/", path, fixed=TRUE)
}

file_remove <- function(paths) {
  paths <- paths[file.exists(paths)]
  if (length(paths) > 0L) {
    file.remove(paths)
  }
}

dirs_create <- function(paths) {
  lapply(paths, dir.create, FALSE, TRUE)
  invisible()
}

## I might want to use curl here because the builtin download is so
## shit.  See what I do in datastorr perhaps.  Another thing that
## would be nice is to offer parallel downloading, but I don't know if
## that is possible (might be worth asking Jeroen).

## TODO: Replace with curl
##' @importFrom utils URLencode download.file
download_file <- function(url, dest) {
  tmp <- tempfile()
  on.exit(file_remove(tmp))
  status <- download.file(URLencode(url), tmp, mode="wb")
  if (status != 0) {
    stop("Download failed with code ", status)
  }
  ok <- suppressWarnings(file.rename(tmp, dest))
  if (!ok) {
    ## Linux
    ok <- file.copy(tmp, dest, overwrite=TRUE)
    if (!ok) {
      stop("Error moving file after downloading")
    }
  }
  invisible(dest)
}

download_file_gunzip <- function(url, dest) {
  tmp <- download_file(url, tempfile())
  on.exit(file_remove(tmp))
  gunzip(tmp, dest)
}

hash_files <- function(files, verbose=FALSE) {
  if (verbose) {
    pb <- progress::progress_bar$new("Hashing: [:bar] :percent",
                                     total=length(files))
    tick <- pb$tick
  } else {
    tick <- function() {}
  }
  f <- function(x) {
    tick()
    tools::md5sum(x)
  }
  vapply(files, f, character(1))
}

read_binary_loop <- function(con, n) {
  res <- raw()
  repeat {
    tmp <- readBin(con, raw(), n)
    if (length(tmp) == 0L) {
      break
    } else {
      res <- c(res, tmp)
    }
  }
  res
}

gunzip <- function(filename, dest) {
  con <- gzfile(filename, "rb")
  on.exit(close(con))
  dat <- read_binary_loop(con, file.size(filename))
  writeBin(dat, dest)
}

string_starts_with <- function(x, y) {
  substr(x, 1, nchar(y)) == y
}

pvapply <- function(fmt, X, FUN, FUN.VALUE, ..., USE.NAMES=TRUE) {
  if (is.null(fmt)) {
    vapply(X, FUN, FUN.VALUE, ..., USE.NAMES=TRUE)
  } else {
    tick <- progress::progress_bar$new(fmt, length(X))$tick
    f <- function(...) {
      tick()
      FUN(...)
    }
    vapply(X, f, FUN.VALUE, ..., USE.NAMES)
  }
}
