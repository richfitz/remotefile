##' Initialise a local remotefile repo
##' @title Initialise local remotefile repo
##' @param path Path to locally store files
##' @param src The URL for the remote
##' @param refresh Force a refresh of the index?
##' @export
remotefile_init <- function(path, src, refresh=FALSE) {
  if (!is_directory(path)) {
    stop("'path' must be an existing directory")
  }
  index_dir <- path_remotefile(path)
  dir.create(index_dir, FALSE)

  ## Check and update the config
  config <- list(source=src)
  filename_config <- path_config(path)
  if (file.exists(filename_config)) {
    config_existing <- jsonlite::fromJSON(filename_config)
    if (!isTRUE(all.equal(config, config_existing))) {
      stop("config mismatch detected")
    }
  } else {
    writeLines(jsonlite::toJSON(config), filename_config)
  }

  filename_index <- path_index(path)
  fetch_index(src, path, refresh)
  ret <- list(path=path, src=src, remote=is_remote(src),
              index=read_index(path))
  class(ret) <- "remotefile"
  ret
}

##' @export
print.remotefile <- function(x, ...) {
  cat("<remotefile>:\n")
  cat(sprintf("  local:  %s\n", x$path))
  cat(sprintf("  remote: %s\n", x$src))
  cat(sprintf("          (%d files)\n", nrow(x$index)))
  invisible(x)
}

##' Check if a path is a remotefile destination
##' @title Check if a path is a remotefile destination
##' @param path Path to test (a directory)
##' @export
is_remotefile_dest <- function(path) {
  file.exists(path_config(path))
}
