## makeCacheMatrix is a list containing a function to 
## set the value of matrix, get the value of matrix
## set the value of inverse and get the value of inverse

makeCacheMatrix <- function(x = matrix()) {       
        m <- NULL                                         
        set <- function(y) {                     
                x <<- y                               
                m <<- NULL                        
        }  
        ## sets the matrix y that you declare by typing 
        ## name_of_the_object_containing_makeCacheMatrix$set(matrix).
        ## Function assignes:
        ## 1. y to object x stored in some environment
        ## 2. NULL to object m stored in some environment
        
        get <- function() x 
        ## function that gets the value of x
        
        setinverse <- function(solve) m <<- solve
        ## function that takes the value of solve and assigns it to m  
        
        getinverse <- function() m
        ## function that gets the value of m   
        
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## This function calculates inverse of a matrix created in makeCacheMatrix.
## If the inverse of declared matrix already exists in cache, 
## it will just print the result, otherwise it will compute the 
## inverse and store it in makeCacheMatrix.

cacheSolve <- function(x, ...) {
        m <- x$getinverse()
        ## gets the value of m from getinverse() 
        ## that's inside of makeCacheMatrix
        
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        ## returns the value of m from makeCacheMatrix if 
        ## it's not NULL and prints the output
        
        data <- x$get()
        ## assigns the value of x to object data
        
        m <- solve(data, ...)
        ## makes inverse of data and stores the output in m 
        
        x$setinverse(m)
        ## saves the output in setinverse defined in makeCacheMatrix
        
        m
}

