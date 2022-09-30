#include <Rcpp.h>
using namespace Rcpp;

//' Calculate Jaccard index for two character vectors
//'
//' @param x character vector
//' @param y character vector
//' @return |intersect(x,y)| / |union(x,y)|
//' @export
// [[Rcpp::export]]
double jaccard_pair( const CharacterVector x, const CharacterVector y){
    int nint=0;
    for( int i=0; i<x.size(); i++){
    for( int j=0; j<y.size(); j++){
        if (x[i] == y[j])
          nint++;
    }
    }
    int denom= x.size() + y.size() - nint ;
    return((double) nint / (double) denom);
}

//' Count overlap between two character vectors
//'
//' @param x character vector
//' @param y character vector
//' @return |intersect(x,y)|
//' @export
// [[Rcpp::export]]
int overlap_pair( const CharacterVector x, const CharacterVector y){
    int nint=0;
    for( int i=0; i<x.size(); i++){
    for( int j=0; j<y.size(); j++){
        if (x[i] == y[j])
          nint++;
    }
    }
    return(nint);
}

//' Calculate Jaccard index for two lists of character vectors
//'
//' @param A list of character vectors
//' @param B list of character vectors
//' @return a matrix of Jaccard indexes with rows corresponding to entries in A,
//' cols to entries in B
//' @export
// [[Rcpp::export]]
NumericMatrix jaccard_lists ( const List A, const List B ) {

  NumericMatrix J(A.size() ,B.size() );
  for(int i=0; i<A.size(); i++) {
    for(int j=0; j<B.size(); j++) {
      SEXP Ae = A[i];
      SEXP Be = B[j];
      J(i,j)=jaccard_pair(Ae,Be);
    }
  }

  return(J);

}

//' Calculate Jaccard index for two lists of character vectors
//'
//' @param A list of character vectors
//' @param B list of character vectors
//' @return a matrix of count of overlaps with rows corresponding to entries in A,
//' cols to entries in B
//' @export
// [[Rcpp::export]]
NumericMatrix overlap_lists ( const List A, const List B ) {

  NumericMatrix J(A.size() ,B.size() );
  for(int i=0; i<A.size(); i++) {
    for(int j=0; j<B.size(); j++) {
      SEXP Ae = A[i];
      SEXP Be = B[j];
      J(i,j)=overlap_pair(Ae,Be);
    }
  }

  return(J);

}

//' Calculate Jaccard indexes for a list of character vector
//'
//' @param A list of character vectors
//' @return a symmetric matrix of Jaccard indexes with rows and cols
//' corresponding to entries in A
//' @export
// [[Rcpp::export]]
NumericMatrix jaccard_symlist ( const List A ) {

  NumericMatrix J(A.size() , A.size() );
  J(0,0)=1;
  for(int i=1; i<A.size(); i++) {
    J(i,i)=1;
    for(int j=0; j<i; j++) {
      SEXP Ae = A[i];
      SEXP Be = A[j];
      double ret=jaccard_pair(Ae,Be);
      J(i,j)=ret;
      J(j,i)=ret;
    }
  }
  return(J);

}

//' Count overlaps for a symmetric list of character vector
//'
//' @param A list of character vectors
//' @return a symmetric matrix of counts with rows and cols
//' corresponding to entries in A
//' @export
// [[Rcpp::export]]
NumericMatrix overlap_symlist ( const List A ) {

  NumericMatrix J(A.size() , A.size() );
  J(0,0)=1;
  for(int i=1; i<A.size(); i++) {
    J(i,i)=1;
    for(int j=0; j<i; j++) {
      SEXP Ae = A[i];
      SEXP Be = A[j];
      double ret=overlap_pair(Ae,Be);
      J(i,j)=ret;
      J(j,i)=ret;
    }
  }
  return(J);

}
