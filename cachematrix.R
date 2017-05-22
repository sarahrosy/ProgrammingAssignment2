## makeCacheMatrix creates a list of previously solved matrix inverses
## the intention is that future matrix inverse calculations will take less time
## because the calculation has already been done previously


## Creates list of matrix inverses

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
      x<<-y
      m<<- NULL
    }
    get <- function(){
      x
    }
    setinv<-function(inv) {
      m<<-inv
    }
    getinv<-function(){
      m
    } 
    list(set=set,get=get,setinv=setinv,getinv=getinv)
}
  

## Either calculates a matrix inverse or fetches the result of a previous matrix inverse calculation, if it exists

cacheSolve <- function(x, ...) {
  m <- x$getinv()
  if(!is.null(m)){
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <-solve(data,...)
  x$setinv(m)
  m
}
