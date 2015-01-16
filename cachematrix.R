## Put comments here that give an overall description of what your
## functions do

## Returns an object ( a list of functions ) which will allow
## us to operate on a matrix in a number of functional ways
## including storing a cached matrix inverse for the given matrix

makeCacheMatrix <- function(x = matrix()) {
    x_inverse = NULL
    list(
        set = function(value) {
            x <<- value
            x_inverse <<- NULL
        },
        get = function(){
            x
        },
        setinverse = function(value){
            x_inverse <<- value
        },
        getinverse = function(){
            x_inverse
        }
    )
}


## for a given cacheMatrix (as returned from makeCacheMatrix)
## either calculate or return a cache inverse.  A cleaner
## method may just use a memoise-pattern in the makeCacheMatrix

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    x_inverse = x$getinverse()
    if(!is.null(x_inverse)) {
        message("getting cached matrix inverse")
        return(x_inverse)
    }
    x_matrix = x$get()
    x_inverse = solve(x_matrix, ...)
    x$setinverse(x_inverse)
    x_inverse
}
