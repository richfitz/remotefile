path_remotefile <- function(path) {
  index <- ".remotefile"
  if (is.null(path)) index else file.path(path, index)
}

path_index <- function(path, compressed=FALSE) {
  file.path(path_remotefile(path),
            if (compressed) "index.json.gz" else "index.json")
}

path_index_hash <- function(path) {
  paste0(path_index(path), ".md5")
}

path_config <- function(path) {
  file.path(path_remotefile(path), "config.json")
}
