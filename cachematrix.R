## The makeCacheMatrix function assigns the inverse of a matrix to a variable so
## it can be cached. This can help speed up run time of the program by
## eliminating the need to calculate the inverse everytime.

## This creates a cache for the matrix

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setInv <- function(inverse) inv <<- inverse
        getInv <- function() inv
        list(set = set, get = get,
             setInv = setInv,
             getInv = getInv)
}


## This checks to see if the inverse of the matrix has been cached. If not it 
## creates and inverse of the matrix and caches it.

cacheSolve <- function(x, ...) {
        inv <- x$getInv()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
            }
            data <- x$get()
            inv <- solve(data,...)
            x$setInv(inv)
            inv
}

