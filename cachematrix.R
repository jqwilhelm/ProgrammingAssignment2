## Matrix inversion is usually a costly computation and there may be 
## some benefit to caching the inverse of a matrix rather than computing 
## it repeatedly.

makeCacheMatrix <- function(x = matrix()) {
    ## This function creates a special "matrix" object that can cache its inverse.
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setsolve <- function(data) m <<- data
    getsolve <- function() m
    list(set = set, get = get,
         setsolve = setsolve,
         getsolve = getsolve)
}

## example
## > myMatrix <- matrix(sample(1:100, 25), nrow=5, ncol=5)
## > mydata <- makeCacheMatrix(myMatrix)


## This function computes the inverse of the special "matrix" returned 
## by makeCacheMatrix above. If the inverse has already been calculated 
## (and the matrix has not changed), then cacheSolve should retrieve 
## the inverse from the cache.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    m <- x$getsolve()
    if(!is.null(m)) {
        message("getting cached matrix data")
        return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setsolve(m)
    m
}

## example
## > cacheSolve(mydata)
##              [,1]         [,2]        [,3]          [,4]        [,5]
## [1,]  0.061585538 -0.038253117  0.03406633  0.0187656360 -0.05767639
## [2,]  0.005405568 -0.019630138 -0.02543081  0.0042174224  0.02027923
## [3,] -0.105060654  0.101210053 -0.04952530 -0.0367915793  0.09083410
## [4,]  0.087584448 -0.083766747  0.03911551 -0.0009565698 -0.04773195
## [5,]  0.005362497 -0.004447548  0.01852509  0.0207315931 -0.02446350

## > cacheSolve(mydata)
## getting cached matrix data
##              [,1]         [,2]        [,3]          [,4]        [,5]
## [1,]  0.061585538 -0.038253117  0.03406633  0.0187656360 -0.05767639
## [2,]  0.005405568 -0.019630138 -0.02543081  0.0042174224  0.02027923
## [3,] -0.105060654  0.101210053 -0.04952530 -0.0367915793  0.09083410
## [4,]  0.087584448 -0.083766747  0.03911551 -0.0009565698 -0.04773195
## [5,]  0.005362497 -0.004447548  0.01852509  0.0207315931 -0.02446350

## You can validate that the function returns the correct values.

## > solve(myMatrix) == cacheSolve(mydata)
## getting cached matrix data
##      [,1] [,2] [,3] [,4] [,5]
## [1,] TRUE TRUE TRUE TRUE TRUE
## [2,] TRUE TRUE TRUE TRUE TRUE
## [3,] TRUE TRUE TRUE TRUE TRUE
## [4,] TRUE TRUE TRUE TRUE TRUE
## [5,] TRUE TRUE TRUE TRUE TRUE

