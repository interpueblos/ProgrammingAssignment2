## The functions serve to return the inverse of a matrix. In case the inverse
## has been previously calculated, the cached value is returned, in the opposite  
## case the inverse is calculated with the solve function

## This function create a list of functions with the basic
## functionallity for the matrix and cached value getter/setter 

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setmatrix <- function(matrix) m <<- matrix
        getmatrix <- function() m
        list(set = set, get = get,
             setmatrix = setmatrix,
             getmatrix = getmatrix)
}


## Check if the inverse has been already calculated, if true the cached value
## is returned. If false calculate with solve function.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        
        m <- x$getmatrix()
        if(!is.null(m)) {
                message("get cache data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setmatrix(m)
        m
}
