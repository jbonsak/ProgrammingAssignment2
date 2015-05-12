## Two functions which together perform matrix inversion when possible, by 
## caching rather than through a resource intensive repeated calculation.


## makeCacheMatrix: Creates a special "matrix" object that can cache its inverse.
## The object is a list containing functions to (1) get the original matrix values,
## (2) set its inverted values and (3) get the inverted values. I reused much of the 
## makeVector excample, but removed the set function which wasn't needed.

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        get <- function() x
        setinv <- function(invert) inv <<- invert
        getinv <- function() inv
        list(get = get, 
             setinv = setinv, 
             getinv = getinv)
}


## cacheSolve: Computes the inverse of the special "matrix" returned by makeCacheMatrix above
## Error is thrown for non-invertible matrices (singular or not square - I found a place to  
## relearn this the fun way at http://www.mathsisfun.com/algebra/matrix-inverse.html).

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getinv()
        if(!is.null(inv)) {      
                message("Using cached inverted matrix")  
                return(inv)
        }
        data <- x$get()
        inv <- solve(data, ...)
        x$setinv(inv)
        inv
}


## Test matrices
## z <- rbind(c(3,4,5), c(6,7,8)) # Example 1 - Has no inverse - is 2x3, not square
## z <- rbind(c(3,4), c(6,8)) # Example 2 - Has no inverse - determinant is 0
## z <- rbind(c(4,7), c(2,6)) # Example 3 - Has inverse (0.6, -0.7), (-0.2, 0.4)

## Test runs
## a <- makeCacheMatrix(z)
## b <- cacheSolve(a) # gets uncached data 
## c <- cacheSolve(a) # reuses the cached data

## Example 3 result
##     [,1] [,2]
##[1,]  0.6 -0.7
##[2,] -0.2  0.4

