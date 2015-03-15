## The following functions calculate the inverse of a matrix and saves it
## to the cache such that the next time the user attempts to calculate the
## matrix inverse, the previously saved value is returned instead of
## repeating the calculation.

## Usage:
##  m <- matrix(1:4, nrow=2, ncol=2)
##  cacheMatrix <- makeCacheMatrix(m)
##  cacheSolve(cacheMatrix)

## This function creates a special "matrix" object, which is really a list 
## containing a function to
## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the value of the inverse
## 4. get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
    ## create a matrix object x and some associated sub-functions/methods
    
    ## define the cache m
    m <- NULL
    set <- function(y) {
        ## assign the input matrix y to the variable x in the
        ## parent environment
        x <<- y
        
        ## re-initialize m in the parent environment to null
        m <<- NULL 
    }
    ## return the matrix x
    get <- function() x 
    
    ## set the cache m equal
    ## to the inverse of the matrix x
    setinverse <- function(inverse) m <<- inverse
    
    ## return the cached inverse of x
    getinverse <- function() m 
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## Return the inverse of an cacheMatrix object

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    m <- x$getinverse()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- solve(data)
    x$setinverse(m)
    m
}
