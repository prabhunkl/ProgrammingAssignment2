## This R file contains two funcations 1) makeCacheMatrix cache the 
## Inversion of a Matrix The 2) cacheSolve to calculate the inversion 
## of the Matrix if it's not available in the cache.

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
    
    inverse <- NULL # set the default inversion to NULL
    
    # set the new matrix and it's inversion ot NULL
    set <- function(y) {
        x <<- y
        inverse <<- NULL
    }
    # retun the Matrix in the cache
    get <- function(){
        x  
    } 
    setInversion <- function(inverted){
        inverse <<- inverted  
    } 
    
    # return the inversion of the matrix cached.
    getInversion <- function(){
        inverse  
    } 
    
    list(set = set, get = get,
         setInversion = setInversion,
         getInversion = getInversion)
}


## cacheSolve will invert the Square matrix and display.
## if the inversion of the matrix already available in the
## cache the same will be retuned.

cacheSolve <- function(x, ...) {
    ## get the inversion of matrix  'x'
    inverse <- x$getInversion()
    
    ## If inversion available print and exit the method
    if(!is.null(inverse)) {
        message("getting cached data")
        return(inverse)
    }
    
    ## If inversion is not availabl in the cache bellow 
    ## lines will be executed.
    
    data <- x$get()
    print(data)
    inverse <- solve(data)
    x$setInversion(inverse)
    inverse
}



## Run the bellow line to test the execution of the above two 
## methods. 

# please make sure you have excuted the method code before executing
# bellow lines.
m = matrix(1:4, nrow=2, ncol=2)
a <- makeCacheMatrix(m)
a$get()
a$getInversion()


cacheSolve(a)



