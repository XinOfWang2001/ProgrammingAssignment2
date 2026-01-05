## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
# This function contains a list of functions to:
# set the value of the matrix
# get the matrix value represented by x
# set the value of the inverse matrix
# get the value of the inverse matrix
# The inv is set to NULL, because no matrix inversion has been calculated yet.
# When calling the function, a function will be returned with all of these functions inside.
# This will be used by the cacheSolve function to get the matrix inversion values from cache.
makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) inv <<- inverse
        getinverse <- function() inv
        list(set = set,
             get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## Write a short comment describing this function

# Answer: The cacheSolve function calculates the inverse of the matrix 'x', after being created by the 'makeCacheMatrix'.
# It will try to retrieve the inverse from memory (cache) to save compution time, in case the calculation was done before.
# If that was the case, the cached value will be returned. Otherwise, it will call the get function to get x, setup earlier at makeCacheMatrix, 
# call the solve function to perform matrix inversion and assigns it to the cache with the setinverse function.
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inverse <- x$getinverse()
        if(!is.null(inverse)) {
                # Existing matrix inversion will be returned from the cache.
                return(inverse)
        } 
        # Otherwise, a matrix inversion will be calculated.
        data <- x$get()
        inverse <- solve(data)
        x$setinverse(inverse)
        return(inverse)
}
