#' @importFrom stats setNames

# Merges two lists without creating duplicate elements
# Used to merge arguments such as includes
merge.list <- function(l1, l2) {
  if (length(l1) * length(l2) == 0) return(c(l1, l2))
  keys <- unique(c(names(l1), names(l2)))
  Map(c, setNames(l1[keys], keys), setNames(l2[keys], keys))
}