## Programming Assignment 2 of Course "R Programming"
## 
## Matrix wrapper to cache its inverse!
## Functions for creating and querying wrapper object
##
## by: t-grab

# Constructor Function for special "matrix" that
# can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
	# "Local" Variable caching the inverse of the matrix
    inv <- NULL
    
    # Setter and Getter for referenced Matrix
    set <- function(y) {
        x <<- y   
        inv <<- NULL
    }
    get <- function() x
    
    # Setter and Getter for cached Inverse (local variable "inv")
    setInverse <- function(inverse) inv <<- inverse
    getInverse <- function() inv
    
    # Return new constructed List containing defined function references
    list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


# Function for computing inverse of special "matrix"
# constructed by "makeCacheMatrix" using its cache

cacheSolve <- function(x, ...) {
    inv <- x$getInverse()
    
    # if inverse is cached return that
    if(!is.null(inv)) {
        message("getting cached data") # just for debug purposes
        return(inv)
    }
    
    # inverse not cached -> calculate it and cache it
    mat <- x$get()
    inv <- solve(mat)
    x$setInverse(inv)
    
    # return cached inverse
    inv
}
