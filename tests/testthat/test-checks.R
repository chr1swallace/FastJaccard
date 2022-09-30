jaccard_R=function(x,y,sep=" ") {
  int=intersect(x,y) %>% length
  uni=union(x,y) %>% length
  int/uni
}

A=list(A1=c("A","B","C"),A2=c("A","D"))
B=list(B1=c("B","C","D"),B2=c("A","E"))

test_that("Rcpp equiv to R", {
  expect_equal(jaccard_R(A[[1]],B[[1]]), jaccard_pair(A[[1]],B[[1]]))
})

test_that("names work", {
  dn=dimnames(jaccard_lists_wrapper(A,B))
  expect_equal(dn, list(names(A),names(B)))
  An=A; Bn=B; names(An)=NULL; names(Bn)=NULL
  dn=dimnames(jaccard_lists_wrapper(An,Bn))
  expect_null(dn)
})

test_that("symlist works", {
  expect_equal(jaccard_symlist(A), jaccard_lists(A,A))
})
