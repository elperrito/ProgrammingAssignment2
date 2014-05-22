## The function makeCacheMatrix creates a special Matrix, 
## which is really a list containing a function to
## * set the value of the Matrix
## * get the value of the Matrix
## * set the inverse of the Matrix
## * get the inverse of the Matrix

## Create a special Matrix contanining a list of 4 functions
## as a example z<-makeCacheMatrix( matrix(1:4,2,2) )
## with z$get(), it displays the matrix assigned to z above (get function)
## with z$set( matrix(3:6,2,2) ), you set z to a different matrix (set function)
## with z$setinverse( solve( matrix(3:6,2,2) ) ), you set inverse value for Z  (setinverse function)
## wiht z$getinverse(), it will display the value of the inverse for z if any, 
## otherwise NULL (getinverse function)

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
      x <<- y
      m <<- NULL
    }
    get <- function() x
    setinverse <- function(solve) m <<- solve
    getinverse <- function() m
    list(set = set, get = get,
          setinverse = setinverse,
          getinverse = getinverse)
}


## Calculates the inverse matrix of a special Matrix created with the above function.
## It checks if the inverse matrix already has been calculated. If so, return the value from get
## from the cache and it skips the calculation.
## If not, then it calculates the inverse of the special Matrix, and it sets the inverse matrix
## in the cache via the setinverse function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    m <- x$getinverse()
    ## check if there is value set for the inverse
    if(!is.null(m)) {
        ## if there is a value, then return that value
        message("getting cached data for matrix inverse")
        return(m)
    }
    ## if there isn't value for inverse, then it calculates the inverse Matrix
    data <- x$get()
    m <- solve(data)
    x$setinverse(m)
    m
}
