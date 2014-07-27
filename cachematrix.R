## makeCacheMatrix function create a special matrix that contail a list
## of functions.
## cacheSolve function uses special matric to calculate inv of matrix

## function to create special matrix whose inv can be cached

makeCacheMatrix <- function(x = matrix()) {
       inv <- NULL
       set <- function(y) {
              x <<- y
              inv <<- NULL
       }
       get <- function() x
       setinv <- function(solve) inv <<- solve
       getinv <- function() inv
       list(set = set, get = get, setinv = setinv, getinv = getinv)
}


## function to compute inv or get cached inv of matrix

cacheSolve <- function(x, ...) {
       inv <- x$getinv()
       if(!is.null(inv)) {
              message("getting cached data")
              return(inv)
       }
       data <- x$get()
       inv <- solve(data, ...)
       x$setinv(inv)
       inv
        ## Return a matrix that is the inverse of 'x'
}
