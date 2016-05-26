##' Create file index
##' @title Create file index
##'
##' @param path Directory to index.  This can be be the current
##'   directory (".") or any other.
##'
##' @param pattern An optional pattern (passed to \code{\link{dir}})
##'
##' @param all.files An optional logical indicating of "dot files"
##'   should be included (passed to \code{\link{dir}})
##'
##' @param recursive An optional logical indicating that the directory
##'   listing should be recursive.
##'
##' @param verbose Be verbose when computing hashes (which can take a
##'   while for very large directories).
##'
##' @return This is called for the side effect of creating an index
##'   file (\code{.remotefile/index.json}) within the directory
##'   \code{path}.
##' @export
create_index <- function(path, pattern=NULL, all.files=FALSE,
                         recursive=FALSE, verbose=TRUE) {
  if (!is_directory(path)) {
    stop("'path' must be an existing directory")
  }
  index_dir <- path_remotefile(path)
  dir.create(index_dir, FALSE)
  files <- dir(path, pattern, all.files, recursive=recursive)
  ## TODO: need to filter out directories here in non-recursive mode
  ## because they can't be hashed.
  if (recursive && all.files) {
    stop("Filtering here not yet supported")
  }
  files <- setdiff(files, path_remotefile(NULL))
  files_full <- normalizePath(file.path(path, files))
  dat <- data.frame(file=files,
                    md5=unname(hash_files(files_full, verbose)),
                    size=file.size(files_full),
                    stringsAsFactors=FALSE)

  index <- path_index(path)
  txt <- jsonlite::toJSON(dat)
  writeLines(txt, index)
  writeLines(txt, gzfile(paste0(index, ".gz")))
  writeLines(unname(hash_files(index, FALSE)), path_index_hash(path))
}

read_index <- function(path, filename=path_index(path)) {
  jsonlite::fromJSON(filename)
}

path_index <- function(path) {
  index <- ".remotefile/index.json"
  if (is.null(path)) index else file.path(path, index)
}

index_file_hash <- function(filename, idx) {
  i <- match(filename, idx$file) # TODO: file -> filename
  if (any(is.na(i))) {
    stop("Files not found in index: ", paste(filename[is.na(i)]))
  }
  idx$md5[i] # TODO: md5 -> hash
}

index_check_file_hash <- function(filename, src, dest, idx) {
  i <- match(filename, idx$file) # TODO: file -> filename
  if (any(is.na(i))) {
    stop("Files not found in index: ", paste(filename[is.na(i)]))
  }
  filename_dest <- file.path(dest, filename)

  hash_dest <- unname(tools::md5sum(filename_dest))
  hash_src <- idx$md5[i] # TODO: md5 -> hash

  file.exists(filename_dest) & hash_dest == hash_src
}

read_index_remote <- function(src, dest, refresh) {
  ## It would be really good if we could read the remote index using
  ## Scott's thing where we try and get the header.  Or if we encode
  ## the first 32 bytes as a hash or something.  Then we could really
  ## efficiently hit the index.  But that's really hard to test
  ## without an internet connection so I'll leave it be for now.

  ## The other thing we can do is keep tabs on when the file was
  ## updated so we can develop some heuristics about when to update
  ## it.
  fetch_index(src, dest, refresh)
  read_index(dest)
}

fetch_index <- function(src, dest, refresh) {
  ## There are at least two things going on here that I'll need to
  ## break up later.  For now just get the main flow working.

  ## TODO: check correct behaviour when running with no internet
  ## connection.

  filename_index <- path_index(dest)
  filename_index_hash <- path_index_hash(dest)

  if (refresh || !file.exists(filename_index)) {
    if (is_remote(src)) {
      has_prev <- file.exists(filename_index_hash)
      md5 <- fetch_index_hash(src, !has_prev)
      if (has_prev && readLines(filename_index_hash) == md5) {
        message("index up to date")
        return(invisible(filename_index))
      }
      download_file_gunzip(path_index(src, TRUE), filename_index)
      if (unname(hash_files(filename_index)) != md5) {
        stop("Unexpected or corrupt index")
      }
      writeLines(md5, filename_index_hash)
    } else {
      ## We'll just always update these ones:
      file.copy(path_index_hash(src), filename_index_hash, overwrite=TRUE)
      file.copy(path_index(src), filename_index, overwrite=TRUE)
    }
  }
  invisible(filename_index)
}

fetch_index_hash <- function(src, required=TRUE) {
  tmp <- tempfile()
  on.exit(file_remove(tmp))
  if (required) {
    download_file(path_index_hash(src), tmp)
  } else {
    res <- tryCatch(download_file(path_index_hash(src), tmp),
                    error=function(e) NULL)
    if (is.null(res)) {
      message("Fetching from internet failed, falling back on existing")
      return(NULL)
    }
  }
  md5 <- readLines(tmp)
  if (!(length(md5) == 1L && nchar(md5) == 32L)) {
    stop("Corrupt hash")
  }
  md5
}
