## Two functions, makeCacheMatrix and cacheSolve, together create a special
## Matrix object which can cache its inverse, and store the inverse in this
## object.

## The first function, makeCacheMatrix, creates a list containing a function to:
## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the value of inverse of the matrix
## 4. get the value of inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) inv <<- inverse
    getinverse <- function() inv
    list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}



## The second function, cacheSolve, returns the inverse of the matrix. It first checks to see if
## the inverse has already been caalculated. If so, it retrieves from the cache and skips the  
## calculation. If not, it calculates and caches the inverse using the setinverse function.

cacheSolve <- function(x, ...) {
    ## Return a matrix, the inverse of 'x'
    inv <- x$getinverse()
    if(!is.null(inv)) {
        message("Retrieving cache.")
        return(inv)
    }
    data <- x$get()
    inv <- solve(data)
    x$setinverse(inv)
    inv
}
