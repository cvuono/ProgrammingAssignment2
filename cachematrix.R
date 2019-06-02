## The pair of funcitons avoids repetitive calculation of a matrix inverse
## by establishing a cache of a previously inverted matrix

## makeCacheMatrix creates the functions to store and retrieve funcitons to a cache

makeCacheMatrix <- function(x = matrix()) {
  xinv <- NULL
  set <- function(y) {
    x<<-y
    xinv<<-NULL
  }
  get <- function() x
  setinverse<-function(invmat) xinv <<- invmat
  getinverse<-function() xinv
  list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}


## cacheSolve retrieves the inverse of a matrix from Cache or calculates it if nothing in Cache

cacheSolve <- function(x, ...){
  xinv<-x$getinverse()
  if(!is.null(xinv)){
    message("getting cached data")
    return(xinv)
  }
  data<-x$get()
  xinv<-solve(data)
  x$setinv(xinv)
  xinv
}
       
