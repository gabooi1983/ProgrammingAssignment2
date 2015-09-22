## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## This function creates a special "matrix" object that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {
        # inv stores the cached inverse matrix
        inv <- NULL

        # Setter for matrix
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        
        # Getter for matrix
        get <- function() x
        
        # Setter for inverse
        setinv <- function(inverse) inv <<- inverse
       
        # Getter for inverse
        getinv <- function() inv
        
        # Return the matrix with new defined functions
        list(set = set, get = get, setinv = setinv, getinv = getinv)
}


## Write a short comment describing this function
## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above.
## If the inverse has already been calculated (and the matrix has not changed), 
## then the cachesolve should retrieve the inverse from the cache.
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
         inv <- x$getinv()

        # If the inverse is already calculated, return it
        if (!is.null(inv)) {
                message("get the cached data")
                return(inv)
        }
        
        # Calculate the inverse if it is not calculated
        data <- x$get()
        inv <- solve(data, ...)
        
        # Cache the inverse
        x$setinv(inv)
        
        # Return the inverse
        inv
}
