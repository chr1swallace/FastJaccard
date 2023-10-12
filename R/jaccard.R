##' Calculate Jaccard index for two lists of character vectors
##'
##' @param A list of character vectors
##' @param B list of character vectors. If B is NULL, sets B=A.
##' (but jaccard_lists(A) is facter than jaccard_lists(A,A))
##' @return a matrix of Jaccard indexes with rows corresponding to entries in A,
##' cols to entries in B
##' @author Chris Wallace
##' A=list(mammals=c("dog","cat"), others=c("fish","spider"))
##' B=list(domestic=c("cat","parrot","dog"), wild=c("lion","spider"))
##' jaccard_lists_wrapper(A,B)
##' @export
jaccard_lists_wrapper=function(A,B=NULL) {
  if(is.null(B)) {
      J = jaccard_symlist(A)
    } else {
      J = jaccard_lists(A,B)
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

##' Calculate denominator of Jaccard index for two lists of character vectors
##'
##' @param A list of character vectors
##' @param B list of character vectors. If B is NULL, sets B=A.
##' (but denom_lists(A) is facter than denom_lists(A,A))
##' @return a matrix of Jaccard indexes with rows corresponding to entries in A,
##' cols to entries in B
##' @author Chris Wallace
##' A=list(mammals=c("dog","cat"), others=c("fish","spider"))
##' B=list(domestic=c("cat","parrot","dog"), wild=c("lion","spider"))
##' denom_lists_wrapper(A,B)
##' @export
denom_lists_wrapper=function(A,B=NULL) {
  if(is.null(B)) {
      J = denom_symlist(A)
    } else {
      J = denom_lists(A,B)
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

##' Calculate overlap between two lists of character vectors
##'
##' @param A list of character vectors
##' @param B list of character vectors. If B is NULL, sets B=A.
##' (but overlap_lists(A) is faster than overlap_lists(A,A))
##' @return a matrix of overlap counts with rows corresponding to entries in A,
##' cols to entries in B
##' @author Chris Wallace
##' @examples
##' A=list(mammals=c("dog","cat"), others=c("fish","spider"))
##' B=list(domestic=c("cat","parrot","dog"), wild=c("lion","spider"))
##' overlap_lists_wrapper(A,B)
##' @export
overlap_lists_wrapper=function(A,B=NULL) {
  if(is.null(B)) {
      J = overlap_symlist(A)
    } else {
      J = overlap_lists(A,B)
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
