## The two functions together serve to cache the inverse of a matrix to avoid
## unnecessary repetitive computations.

## The makeCacheMatrix function creates a "personal profile" for a certain
## matrix. It can store a matrix and its inverse.

makeCacheMatrix <- function(x = matrix()) {
  inverse<-NULL
  set<-function(y){
    x <<- y
    inverse<<-NULL
  }
  get<-function(){x}
  setInv<-function(inver=matrix()){inverse<<-inver}
  getInv<-function(){inverse}
  
  list(set=set,get=get,setInv=setInv,getInv=getInv)
}

## The CacheSolve function first checks whether the inverse of the input matrix
## has been calculated. If so, it will directly return the inverse matrix. Othe-
## rwise, it will first calculate the inverse of the matrix and then return it.

cacheSolve <- function(x, ...) {
    inverse <- x$getInv
    if (!is.null(inverse)){inverse}
    else{
      inverse<-solve(x$get())
      x$setInv(inverse)
      inverse
    }
}

