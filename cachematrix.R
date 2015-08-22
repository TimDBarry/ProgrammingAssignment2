# Function List:
# makeCacheMatrix - Given a matrix, will store it and provide methods to store it's inverse as well.
# cacheSolve - Given a matrix, will calculate it's inverse for the first call and cache it.
#       Any subsequent calls will retrieve the cached data rather than re-calculating.


makeCacheMatrix <- function(x = matrix()) {
    # Purpose: Given a matrix, will store it and provide methods to store it's inverse as well.
    #  
    # Methods:
    #    set - records a matrix to the internal matrix variable "m".
    #    get - returns the internal matrix variable "m".
    #    setinverse - records a matrix inverse to the internal matrix variable "m".
    #    getinverse - returns the internal matrix variable "m".
    #
    #Example:
    # mymat <- makeCacheMatrix()                        #Construct a new Matrix object.
    # mymat$set(matrix(c(3,1,5,2,-3,-1,-5,2,4), 3, 3))  #Assign Matrix object's matrix.
    # mymat$get()                                       #Retrieve the matrix out of the object.
    # mymat$setinverse(solve(matrix(c(3,1,5,2,-3,-1,-5,2,4), 3, 3))))  #Assign the Matrix object's inverse.
    # mymat$getinverse()                                #Retreive the Matrix object's inverse.
    
    m <- NULL
    
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    
    get <- function() {x}
    
    setinverse <- function(matrixinverse) {m <<- matrixinverse}
    
    getinverse <- function() {m}
    
    list(
        set = set, 
        get = get,
        setinverse = setinverse,
        getinverse = getinverse
    )
    
}



cacheSolve <- function(x, ...) {
    # Purpose: Given a matrix, will calculate it's inverse for the first call and cache it.
    #       Any subsequent calls will retrieve the cached data rather than re-calculating the inverse.
    #  
    # Methods:
    #
    #Example:
    # mymat <- makeCacheMatrix()                        #Construct a new Matrix object.
    # mymat$set(matrix(c(3,1,5,2,-3,-1,-5,2,4), 3, 3))  #Assign Matrix object's matrix.
    # cacheSolve(mymat)
    # cacheSolve(mymat)
    
            ## Return a matrix that is the inverse of 'x'
    m <- x$getinverse()
    
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    
    data <- x$get()
    
    m <- solve(data, ...)
    
    x$setinverse(m)
    
    return(m)    
}
