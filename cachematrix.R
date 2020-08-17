

makeCacheMatrix <- function(x = matrix()) { ## to cache invers matrix into m in main environment
    m <- NULL ## m in current environment
    setmatr <- function(y) { 
      x <<- y    ## y in parent environment
      m <<- NULL ## m in parent environments
    }
    getmatr <- function() x  ## get from cache
    setInvMatr <- function(solve) m <<- solve  ## cache inv matrix
    getInvMatr <- function() m                 ## get inv matrix from cache
    list(setmatr = setmatr, getmatr = getmatr, 
         setInvMatr = setInvMatr,
         getInvMatr = getInvMatr)
}
  


cacheSolve <- function(x, ...) {  ## Return inverted matrix of 'x', calculated or got from cache
  m <- x$getInvMatr() # get inv matrix if already calculated
  if(!is.null(m)){ # if not first time run
    message("getting cached data")
    matrix<-x$getmatr()
    m<-solve(matrix, ...)
    x$setmatr(m)
    return(m) 
  }
    # when first time  
    y <- x$getmatr() # get matrix value
    x$setmatr(y) # cache matrix value
    m <- solve(y, ...) # inverse matrix
    x$setInvMatr(m) # cache inverse matrix
    m
  
}


