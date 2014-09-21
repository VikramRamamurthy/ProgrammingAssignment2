## The first function, `makeCacheMatrix` creates a special list fl, which is
## really a list containing a function to
##1.  set the value of the matrix
##2.  get the value of the matrix
##3.  set the value of the inverse of the matrix
##4.  get the value of the inverse of the matrix
## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y = matrix()) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x 
        setinverse <- function(value_inv) inv <<- value_inv
        getinverse <- function() inv
        fl<-list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


##The following function calculates the inverse of the special matrix
##created with the above function. However, it first checks to see if the
##mean has already been calculated and that the matrix has not changed. 
##If so, it `get`s the inverse  from the cache and skips the computation.
##Otherwise, it calculates the inverse of the new matrix after setting 
##the new value of the matrix via 'set' function to the special matrix  
##and sets the value of the mean in the cache via the `setmean`function.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        cacheM<-fl$get()
        inv <- fl$getinverse()
        ##checks if inverse has already been calculated and
        ##the matrix has not changed.
        if(!is.null(inv) && (sum(as.vector(cacheM) == as.vector(x)) == length(x))) {          
                message("getting cached data")
                return(inv)
        }
        ##Sets the new matrix to the special list
        newM <- fl$set(x)
        inv <- solve(x, ...)
        fl$setinverse(inv)
        inv
}


