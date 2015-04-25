## Below are two functions that are used to create a special object
## that stores a matrix and caches its inverse

## This function creates a list that contains a function to
## 1. Set the value of the matrix
## 2. Get the value of the matrix
## 3. Set the value of the matrix inverse
## 4. Get the value of the matrix inverse

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setinv <- function(solve) inv <<- solve
        getinv <- function() inv
        list (set = set, 
              get = get, 
              setinv = setinv,
              getinv = getinv)
}


## Calculates the inverse of a matrix.  It first checks if an 
## inverse has already been calculated.  If so, it gets the inverse
## from the cache and skips the computation.  Otherwise, it
## calculates the inverse of the matrix and sets the value of the
## inverse in the cache via the setinv function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getinv()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        datamatrix <- x$get()
        inv <- solve(datamatrix, ...)
        x$setinv(inv)
        inv
}
