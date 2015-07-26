## The purpose of the following program is to produce the
## inverse of a matrix

## The makeCacheMatrix outputs 4 functions that will be
## used in CacheSolve. I tried writing comments below
## that hopefully explain why each portion is included.

makeCacheMatrix <- function(x = matrix()) {
        
        ## Have to define m so that it can be used.
        ## Currently it is null.
        m <- NULL
        
        ## Cache the matrix for future use.
        set <- function(y){
                x<<-y
                m<<-NULL
        }
        
        ## Returns the value of the matrix, whether
        ## it is cached or the original matrix.
        get <-function()x
        
        ## Caches the inverse matrix for future
        ## use.
        setinverse<-function(solve)m<<-solve
        
        ## Gets the desired inverse matrix.
        getinverse<-function()m
        
        ## This is simply a list of all the functions
        ## we just defined. Redefining them here so 
        ## they can be used in the cacheSolve function.
        list(set=set, get=get,
             setinverse=setinverse,
             getinverse=getinverse)
}

## CacheSolve is used to find the inverse matrix of
## interest. The input for this function is what is
## defined in makeCacheMatrix. Like above, I tried
## putting in comments for each portion to hopefully
## explain what is going on.

cacheSolve <- function(x, ...) {
        
        ## Checks to see if inverse is already 
        ## calculated from the cache matrix. If
        ## so, "m" will be returned.
        m<- x$getinverse()
        
        ## If the inverse is not calculated,
        ## the function goes to the cached "x"
        ## to calculate.
        if(!is.null(m)){
                message("getting cached data")
                return(m)
        }
        
        ## Sets "data" to the matrix defined in
        ## the get function. This can either be
        ## the original inputted matrix, or the
        ## cached one from the set function.
        data<-x$get()
        
        ## Calculates the defined matrix.
        m<-solve(data,...)
        
        ## Gives the inverse matrix.
        x$setinverse(m)
        
        ## Creates the final output as the inverse
        ## matrix.
        m
}