## The following functions first create a special matrix object, that can cache the inverse of the matrix, 
## if the inverse was calculated by the seond function cacheSolve.
## The function Cachesolve will calculate the inverse of the matrix, when the function is called for the first time, 
## otherwise the function will return the inverse of the matrix from the cache.


## This function creates a special matrix object, that stores the matrix and can store in addition the inverse of the matrix,
## if the inverse was calculated by the function cacheSolve

makeCacheMatrix <- function(x = matrix()) {

        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setsolve <- function(solve) m <<- solve
        getsolve <- function() m
        list(set = set, get = get,
             setsolve = setsolve,
             getsolve = getsolve)

}


## The function will access a special matrix object, created by the makeCacheMatrix function 
## and will return the inverse of the matrix. If the inverse is stored within the special matrix object,
## the inverse of this matrix object will be returned, otherwise it will be calculated and stored in the 
## special matrix object

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getsolve()
        if(!is.null(m)) {
                message("getting cached inverse matrix")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setsolve(m)
        m
}


## Examples:
## matrix_1 <- matrix(1:4, 2, 2)
## special_mat_1 <- makeCacheMatrix(matrix_1)
## cacheSolve(special_mat_1)
## cacheSolve(special_mat_1)
## cacheSolve(special_mat_1)

