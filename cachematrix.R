#FUNCTION TO CREATE A SPECIAL MATRIX WHICH IS A LIST CONTAINING 
#FUNCTIONS TO SET, GET VALUE OF A MATRIX. AND TO SET AND GET INVERSE
#OF A MATRIX 

makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    set <- function(y) {
        x <<- y
        i <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) i <<- inverse
    getinverse <- function() i
    list(
        set = set,
        get = get,
        setinverse = setinverse,
        getinverse = getinverse)
}




#TO CALCULATE INVERSE OF THE MATRIX
#IF INVERSE IS ALREADY CALCULATED, GET THE INVERSE FROM CACHE
#ELSE, TO CALCULATE THE INVERSE AND SET IT IN THAT CACHE

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    i <- x$getinverse()
    if(!is.null(i)) {
        message("Getting Cached Data")
        return(i)
    }
    data <- x$get()
    i <- solve(data, ...)
    x$setinverse(i)
    i
}