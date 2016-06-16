# Merges two lists without creating duplicate elements
# Used to merge arguments such as includes or css (HTML)
merge.list <- function(l1, l2) {
  if (length(l1) == 0) return(l2)
  if (length(l2) == 0) return(l1)
  keys <- unique(c(names(l1), names(l2)))
  Map(c, setNames(l1[keys], keys), setNames(l2[keys], keys))
}