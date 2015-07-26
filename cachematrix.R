


## makeCacheMatrix - function creates a matrix and sets inverse calculation in cache

makeCacheMatrix <- function(x = matrix()) {
  m<-NULL
  set<-function(y){
  x<<-y
  m<<-NULL
}

get<-function() x
setmatrix<-function(solve) m<<- solve
getInverse<-function() m
list(set=set, get=get,
   setmatrix=setmatrix,
   getInverse=getInverse)
}


## cacheSolve - function computes inverse of matrix

cacheSolve <- function(x=matrix(), ...) {
    m<-x$getInverse()
    if(!is.null(m)){
      message("getting cached data")
      return(m)
    }
    matrix<-x$get()
    m<-solve(matrix, ...)
    x$setmatrix(m)
    m
}