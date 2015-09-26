## The following are two factions that assign some values to an 
## object. The former functions is dependent on the calculation 
## of the latter and caches the matrix inverse from it

## This function will create a special matrix object that will 
## cache the inverse of a matrix

makeCacheMatrix <- function(x = matrix()) {
       inverse_X <- NULL
       set <- function(y) {
               x <<- y
               inverse_x <<- NULL
       }
       get <- function() x
       setmatrix <- function(solve) inverse_X <<- solve
       getmatrix <- function() inverse_x
       list(set = set, get = get, setmatrix = setmatrix,
              getmatrix = getmatrix)
}


## cacheSolve returns the inverse of a matrix specified in makeCachmatrix.
## If the inverse has already been calculated cachSolve will retrieve 
## the retrieve the inverse from cache. 

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
       inverse_x <- x$getinverse()
       if (!is.null(inverse_x)) {
               message("getting inverse matrix")
               return(inverse_x)
       }
       data = x$get()
       inverse_x = solve(data, ...)
       x$setmatrix(inverse_x)
}
