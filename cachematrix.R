## cachematrix.R implements a matrix object that allows
## caching of the inverse results using the getinv and setinv functions

## makeCacheMatrix returns a list of functions that allow get and set for both
## the matrix data and the inverse.
## The only input is x which has to be a square matrix in order to be invertible
makeCacheMatrix <- function(x = matrix()) {
    ## When creating the object, its inverse is not yet calculated
    inv = NULL
    ## set(y) stores the matrix
    set <- function(y)
    {
        x <<- y
        inv <<- NULL
    }
    ## get() returns the stored matrix
    get <- function() x
    ## setinv() stores the inverse into inv
    setinv <- function(input_inv) inv <<- input_inv
    ## getinv() returns the inverse previously stored
    getinv <- function() inv
    
    list(set=set, 
         get=get,
         setinv=setinv,
         getinv=getinv)
}


## cacheSolve makes use of the makeCacheMatrix function and determines if the
## inverse of the matrix has to be calculated. If the inverse has been
## previoulsy cached, it just retrieves the data and returns it. If not,
## the inverse is calculated, cached and returned.
cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    m <- x$getinv()
    if (!is.null(m))
    {
        ## if the inverse existis in x, just return it and tell the user
        message('Getting cached inverse')
        return(m)
    }
    ## if no inverse has been calculated previously:
    # 1) store the matrix in mat
    mat = x.get()
    ## 2) compute the inverse
    m = solve(mat)
    ## 3) store the inverse in the new custom matrix object
    x$setinv(m)
    ## 4) return the inverse
    m
}