## These two functions together provide a functionality to return 
## cached value of the inverse of the matrix so that it doesn't have
## to be computed repeatedly.

## This function encapsulates a matrix and its inverse and 
## provide getters and setters for the same. Everytime this 
## function receives a new matrix, it nullifies the existing 
## value of the inverse.

makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    set <- function(y) {
        x <<- y
        i <<- NULL
    }
    get <- function() x
    setinverse <- function(inv) i <<- inv
    getinverse <- function() i
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)

}


## This function takes the output of makeCacheMatrix function as an
## argument and returns the inverse of the matrix encapsulated in the
## input. It tries to return the cached value of the inverse and if 
## it is null, it computes the inverse, puts it in the cache and 
## returns it.

cacheSolve <- function(x) {
    i <- x$getinverse()
    if(!is.null(i)) {
        message("getting cached data")
        return(i)
    }
    data <- x$get()
    i <- solve(data)
    x$setinverse(i)
    i
}
