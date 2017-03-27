## The makeCacheMatrix file has two functions to aide in the processing matrices. 
## one function creates a cache of a matrix that you provide, the second function
## creates the inverse of the function and adds it to the cache. 

## This creates a cache of the matrix

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
## creates an inverse of the matrix and adds it to the cache.
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

