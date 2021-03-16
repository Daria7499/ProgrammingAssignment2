makeCacheMatrix <- function(x = matrix()){
  invrs = NULL
  set <- function(y){
    x <<- y
    invrs <<- NULL
  }
  get <- function() x
  setinvrs = function(inverse) invrs <<- inverse
  getinvrs = function() invrs
  list(set = set, 
       get = get, 
       setinv = setinvrs, 
       getinv = getinvrs)
}

cacheSolve <- function(x,...){
  invrs = x$getinv()
  if (!is.null(invrs)){
    message("getting cached data")
    return(invrs)
  }
  data = x$get()
  invrs = solve(data,...)
  x$setinv(invrs)
  invrs
}

matr <- matrix(c(1,2,3,4), 2, 2)
matr
rrr <- makeCacheMatrix(matr)
cacheSolve(rrr)






