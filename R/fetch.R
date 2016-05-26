##' Fetches remote files.
##' @title Fetch a remote file
##'
##' @param filename The name of a file, to be found at the remote
##'   location \code{obj}.  Do not include the first part of the URL.
##'
##' @param obj A \code{remotefile} object, as created by
##'   \code{\link{remotefile_init}}
##'
##' @return The filename, on your computer
##' @export
remotefile_fetch <- function(filename, obj) {
  if (length(filename) > 1L) {
    return(vapply(filename, remotefile_fetch, character(1), obj))
  }
  if (!inherits(obj, "remotefile")) {
    stop("'obj' must be a remotefile object")
  }
  filename <- fix_windows_paths(filename)
  filename_dest <- file.path(obj$path, filename)
  ok <- index_check_file_hash(filename, obj$src, obj$path, obj$index)
  if (ok) {
    filename_dest
  } else {
    dirs_create(dirname(filename_dest))
    filename_src <- file.path(obj$src, filename)
    if (obj$remote) {
      download_file(filename_src, filename_dest)
    } else {
      file.copy(filename_src, filename_dest)
    }
  }
  filename_dest
}

##' List remote files
##' @title List remote files
##'
##' @param obj A \code{remotefile} object, as created by
##'   \code{\link{remotefile_init}}
##'
##' @param pattern Optional pattern, as a regular expression.  Will be
##'   interpreted by \code{grep} or \code{dir}.
##'
##' @param local Look in the local directory?
##' @export
remotefile_list <- function(obj, pattern=NULL, local=FALSE) {
  if (local) {
    dir(obj$path, pattern)
  } else {
    files <- obj$index$file
    if (!is.null(pattern)) {
      files <- files[grep(pattern, files)]
    }
    files
  }
}
