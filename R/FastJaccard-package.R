#' @useDynLib FastJaccard, .registration = TRUE
#' @exportPattern "^[[:alpha:]]+"
#' @importFrom Rcpp evalCpp
NULL

##' Calculate Jaccard index for two lists of character vectors
##'
##' @param A list of character vectors
##' @param B list of character vectors. If B is NULL, sets B=A.
##' (but jaccard_lists(A) is facter than jaccard_lists(A,A))
##' @return a matrix of Jaccard indexes with rows corresponding to entries in A,
##' cols to entries in B
##' @author Chris Wallace
##' @export
jaccard_lists=function(A,B=NULL) {
  if(is.null(B)) {
      J = jaccard_symlist_cpp(A)
    } else {
      J = jaccard_lists_cpp(A,B)
    }
  ## names
  if(!is.null(names(A))) {
    rownames(J)=names(A)
    if(is.null(B))
      colnames(J)=names(A)
  }
  if(!is.null(B) && !is.null(names(B)))
    colnames(J)=names(B)

  J
}
