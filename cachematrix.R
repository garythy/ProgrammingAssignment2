
## makeCacheMatrix is a function that stores a list of functions
## makeCacheMatrix contains 4 functions: set, get, setinverse, getinverse
## set changes the vector stored in the main function
## get returns the vector x stored in the main function
## setinverse stores the value of the input in m into the main function
## getinverse returns value of m

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {                            
                x <<- y
                m <<- NULL
        }
        get <- function() x                             
        setinverse <- function(inverse) m <<- inverse   
        getinverse <- function() m                      
        list(set = set, get = get,                      
             setinverse = setinverse,
             getinverse = getinverse)
}


## verifies value m stored with getinverse, if exists, return m, or else get vector stored with makeCacheMatrix, 
## calculates inverse and store it in the object generated assigned with makeCacheMatrix

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getinverse()                             
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()                                 
        m <- solve(data, ...)
        x$setinverse(m)
        m
}
