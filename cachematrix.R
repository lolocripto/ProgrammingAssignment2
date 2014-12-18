## makeCacheMatrix create a special "matrix" list wich contains a function to



## This function creates a special "matrix" object that can cache its inverse matrix with 4 different method or function.
# set the value of the matrix
# get the value of the matrix
# set the value of the inverse matrix
# get the value of the inverse matrix
makeCacheMatrix <- function(x = matrix()) {
    # ini the inverse matrix
    m <- NULL
    
    #set the value of the matrix
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    #get the value of the matrix
    get <- function() x
    #set the value of the inverse matrix
    setinv <- function(inv) m <<- inv
    #get the value of the inverse matrix
    getinv <- function() m
    #return a list of all the above funtion
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), 
## then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of the matrix 'x' 
    # if exist in cache.
    m <- x$getinv()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    # get the original matrix
    data <- x$get()
    # calculate the inverse matrix
    m <- solve(data, ...)
    # store the inverse matrix in cache
    x$setinv(m)
    # return the inverse matrix
    m
}
