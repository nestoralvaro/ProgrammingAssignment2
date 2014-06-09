## Matrix inversion is usually a costly computation and their may 
## be some benefit to caching the inverse of a matrix rather than 
## compute it repeatedly.
## These two functions can cache the inverse of a matrix.


## This function creates a special "matrix" object
## This "matrix" object stores a cached copy of its inverse once it's calculated
makeCacheMatrix <- function(x = matrix()) {
        ## In the beginning there's no inverse matrix
        inv <- NULL
        ## Prepare my special "matrix"
        set <- function(...) {
                ## Create the matrix
                x <<- matrix(...)
                ## Reset the inverse
                inv <<- NULL
        }
        ## Get the stored matrix
        get <- function() x
        ## Store "inverse" as the inverse of the matrix
        setinverse <- function(inverse) inv <<- inverse
        ## Retrieve the value of "inv" (inverse of the matrix)
        getinverse <- function() inv
        ## Return a list with the 4 exposed methods
        list(set = set, get = get, 
             setinverse = setinverse,
             getinverse = getinverse)
}


## This function computes the inverse of the special "matrix" object
## Once the inverse has been calculated it's stored on that "matrix" object
cacheSolve <- function(x, ...) {
        ## Get the information already stored
        inv <- x$getinverse()
        ## If there's stored information return it
        if(!is.null(inv)) {
                message("getting cached data")
                ## Exit the method
                return(inv)
        }
        ## If we reach this point, calculate the inverse
        ## Get the matrix
        data <- x$get()
        ## Calculate the inverse of such matrix
        inv <- solve(data)
        ## Set the inverse information (caches the information)
        x$setinverse(inv)
        ## Return the inverse matrix
        inv
} 