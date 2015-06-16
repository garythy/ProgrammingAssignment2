
## makeCacheMatrix is a function that stores a list of functions
## makeCacheMatrix contains 4 functions: set, get, setinverse, getinverse

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {                            ## changes the vector stored in the main function
                x <<- y
                m <<- NULL
        }
        get <- function() x                             ## returns the vector x stored in the main function
        setinverse <- function(inverse) m <<- inverse   ## store the value of the input in m into the main function
        getinverse <- function() m                      ## return value of m
        list(set = set, get = get,                      ## store all four functions in the main function
             setinverse = setinverse,
             getinverse = getinverse)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getinverse()                             ## verifies value m stored with getinverse, if exists, return m
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()                                 ## or else get vector stored with makeCacheMatrix, calculates inverse and store it in the object generated assigned with makeCacheMatrix 
        m <- solve(data, ...)
        x$setinverse(m)
        m
}
