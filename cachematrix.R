## R Programming Assignment 2: Lexical Scoping--caching of the inverse of a matrix
## The function makeCacheMatrix creates a special "matrix"
## This is a list containing a function to

## 1. set the value of the matrix  
## 2. get the value of the matrix 
## 3. set the value of the inverse of the matrix - setinverse
## 4. get the value of the inverse of the matrix - getinverse



makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
            x <<- y
            inv <<- NULL

    }
    get <- function()x
    setInverse <- function(inverse) inv <<- inverse
    getInverse <- function() inv
    list(set = set, get = get, 
         setInverse = setInverse, 
         getInverse = getInverse)

}  
## The function cacheSolve calculates the value of the inverse of the a special "matrix"
## from the argument of the makeCacheMatrix function above
## If first checks to see if the inverse has been calculated and if so, it 
## retrieves the inverse fron the cache
## If the inverse has not been calculated the inverse is then calculated via the solve function
## and the value of the inverse is set in the cache via the setInverse function

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
    inv <- x$getInverse()
    if(!is.null(inv)) {
            message("getting cached data")
            return(inv)
    }
    data <- x$get()
    inv <- solve(data, ...)
    x$setInverse(inv)
    inv
}
