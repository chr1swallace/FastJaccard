logsum = function(x) {
  my.max <- max(x)                              ##take out the maximum value in log form
  my.res <- my.max + log(sum(exp(x - my.max )))
  return(my.res)
}
logdiff = function(x,y) {
  my.max <- max(x,y)                              ##take out the maximum value in log form
  my.res <- my.max + log(exp(x - my.max ) - exp(y-my.max))
  return(my.res)
}

ARI=function(A,B,use.logs=FALSE) {
  if(!is.list(A) || !is.list(B))
    stop("A and B must be lists of character vectors")
  nij=overlap_lists(lapply(A,as.character),
                    lapply(B,as.character))
  nr=rowSums(nij)
  nc=colSums(nij)
  if(use.logs) {
    logsumr=logsum( lchoose(nr,2) )
    logsumc=logsum( lchoose(nc,2) )
    logn=lchoose(sum(nr),2)
    logsumn=logsum( lchoose(nij,2) )
    offset=logsumr + logsumc - logsumn
    lognumer=logdiff( logsumn, offset)
    logdenom=logdiff( log(0.5) + logsum(c(logsumr,logsumc)), offset)
    exp(lognumer - logdenom)
  } else {
    sumr=sum(choose(nr,2))
    sumc=sum(choose(nc,2))
    n=choose(sum(nr),2)
    sumn=sum( choose(nij,2))
    offset=sumr * sumc / sumn
    (sumn - offset) / (0.5 * (sumr + sumc) - offset)
  }
}
