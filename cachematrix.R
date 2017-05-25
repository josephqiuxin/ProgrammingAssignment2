## Put comments here that give an overall description of what your
## functions do

## The makeCacheMatrix is used here to create a special subject that can store a matrix and cache its inverse. 
## It takes an invertible matrix as an input. 'inv' is the vector that is used to store the inverse matrix. 
## set sets the variable x to a new matrix y which is an input by the user
## get returns the stored matrix 'x'.
## setinverse assigns the inverse of matrix 'x' that is given as an input 'y' by the user to the variable 'inv'.
## getinverse returns the matrix stored in the variable 'inv'.

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


## This cacheSolve function takes a list of 'x' as an input that was returned by the function above 
## and returns the inverse matrix. 
## The function assigns the variable 'inv' the  value of inverse stored in the previous function using getinverse function.
## If the value is not NULL, the function returns the cached matrix and stops. 
## If the inverse hasn't been calculated, the function calls the get function and stores the matrix in the variable 'mat'.
## If it calculates the inverse of the matrix later and stores it in the variable 'inv' of the 'cacheSolve' function 
## as well as in the 'makeCacheMatrix' function by calling the 'setinverse' function so that it can be used in future.
## Finally it returns the value of the inverse of the matrix.

cacheSolve <- function(x, ...) {
        inv <- x$getinverse()
        if (!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        mat <- x$get()
        inv <- solve(mat, ...)
        x$setinverse(inv)
        inv
        ## Return a matrix that is the inverse of 'x'
}
