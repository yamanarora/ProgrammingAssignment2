## Here are two functions that are aimed at getting the inverse of a matrix (assumed to be invertible). The aim is to check the cache
## for an existing solution, and if solution is not present, then calculate the inverse and store it in the cache.

## The function makeCacheMatrix accepts a matrix whose inverse is to be found as input. It creates a matrix to store its inverse in
## the cache.


makeCacheMatrix <- function(x = matrix()) {
  xinv<-NULL
  set<-function(y){
  x<<-y
  xinv<<-NULL
}
get<-function() x
setmat<-function(solve) xinv<<- solve
getmat<-function() xinv
list(set=set, get=get,
   setmat=setmat,
   getmat=getmat)
}

## The function cacheSolve checks if the inverse of the matrix required already exists in the cache or not. If it exists,
## it is returned, and if the inverse does not exist, it calculates the inverse.

cacheSolve <- function(x=matrix(), ...) {
    xinv<-x$getmat()
    if(!is.null(xinv)){
      message("getting cached data")
      return(xinv)
    }
    matrix<-x$get()
    xinv<-solve(matrix, ...)
    x$setmat(xinv)
    xinv
}