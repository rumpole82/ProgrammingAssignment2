## These functions will save time when computing the inverse of a large matrix multiple times.

## The makeCacheMatrix() function is passed a matrix, which is used to create, then access, 
## an object.

makeCacheMatrix <- function(x = matrix()) {     ## Input x will be a matrix.
    inv <- NULL                 ## inv will be the inverse of matrix x. It is
                                ## reset to NULL every time makeCacheMatrix() is called.
    
    ## This function lets you assign a new value to the object, in order to save memory.
    set <- function(y) {        ## Takes an input matrix.
        x <<- y                 ## Saves the input matrix.
        inv <<- NULL            ## Resets inv to NULL.
    }
    
    get <- function() x         ## Returns the value of the original matrix.
    setsolution <- function(solve) inv <<- solve  ## Called by cacheSolve() the first time 
                                                  ## it is run for x and stores that 
                                                  ## value in the parent environment, 
                                                  ## a process called superassignment.
    getsolution <- function() inv       ## Returns cached value on subsequent runs of 
                                        ## cacheSolve().
    list(set = set, get = get,      ## Accessed each time makeCacheMatrix() is called to make
         setsolution = setsolution, ## a new object. It's a list of internal functions used 
         getsolution = getsolution) ## by cacheSolve(), not by makeCacheMatrix().
}

## cacheSolve() will solve for the inverse of the matrix you gave to makeCacheMatrix() or
## simply return the value if it has already been calculated.

cacheSolve <- function(x, ...) {    ## Input x is an object created by makeCacheMatrix().
    inv <- x$getsolution()          ## Access object x and get value of inverse matrix.
    if(!is.null(inv)) {             ## Find out if inv is not NULL, which means that it 
                                    ## has been calculated already.
        message("getting cached data")  ## If TRUE, prints this message,
        return (inv)                ## and returns its value.
        }
        data <- x$get()             ## If FALSE, it "gets" the original matrix
                                    ## in the list created by makeCacheMatrix(),
        inv <- solve(data, ...)     ## then solves for its inverse,
        x$setsolution(inv)          ## and assigns this solution to the list created by 
                                    ## makeCacheMatrix().
        inv        ## Returns a matrix that is the inverse of 'x'.
}
