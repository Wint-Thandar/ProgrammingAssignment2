# A pair of functions that cache the inverse of a matrix.


# This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) { 
    i <- NULL               # initialize variable 'i' to hold the value of matrix inverse
    set <- function(y) {    # define the set function to assign a new value of matrix in parent environment
        x <<- y             # set value of matrix in parent environment
        i <<- NULL          # reset i to NULL value if there is a new matrix
    }
    get <- function() x     # define the get function to return the x value of the matrix argument
    
    setinverse <- function(inverse) i <<- inverse  
    getinverse <- function() i                     
    list(set = set, get = get, 
         setinverse = setinverse, 
         getinverse = getinverse)
}


# This function computes the inverse of the special "matrix" returned by makeCacheMatrix function above.
# If the inverse has already been calculated (and the matrix has not changed), then this function will retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    i <- x$getinverse()
    if(!is.null(i)) {
        message("getting cached data")
        return(i)
    }
    data <- x$get()
    i <- solve(data, ...)
    x$setinverse(i)
    i
}
