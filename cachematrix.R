## The following two functions will allow one to cache 
## the inverse of a square invertible matrix after solving it
##
## The first function, makeCacheMatrix, creates a list object 
## aka "a special 'matrix' object"
## containing functions that can cache the inverse of a 
## square invertible matrix
##
## The second function, cacheSolve, returns the inverse of the matrix
## using the list object returned by makeCacheMatrix. If the inverse
## has been cached, it will return that. If not, it will solve the inverse,
## cache it, and return it.

##

## makeCacheMatrix will generate a list of functions used
## to cache the inverse of an invertible matrix. Specifically, the list
## contains functions to:
## 1. set the value of the matrix (set)
## 2. get the value of the matrix (get)
## 3. set the value of the inverse (setinverse)
## 4. get the value of the inverse (getinverse)
## To use makeCacheMatrix properly: pass a square invertible matrix to it
## Then pass the returned list object to the cacheSolve function

makeCacheMatrix <- function(x = matrix()) {
        
        ## assign initial value of i to NULL
        i <- NULL
        
        ## define set to be a function that assigns y to x 
        ## and i to NULL in parent environment 
        set <- function(y) {
                x <<- y
                i <<- NULL
        }
        
        ## assign function to get that returns x
        get <- function() x
        
        ## assign function that is passed the inverse and assigns the
        ## inverse to i in the parent environment 
        setinverse <- function(inverse) i <<- inverse
        
        ## assign function to getinverse that returns i
        getinverse <- function() i
        
        ## store the above functions in list and return the list
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)

}


## cacheSolve returns the inverse of the matrix originally
## passed to makeCacheMatrix using the list object
## returned from makeCacheMatrix. It first checks to see if the 
## inverse has already been determined. If so, it gets the inverse from 
## the cache and skips computing it. Otherwise, it computes the inverse 
## of the matrix and sets the inverse in the cache 
## via the setinverse function.
## To use cacheSolve properly pass it the object returned by makeCacheMatrix

cacheSolve <- function(x, ...) {
        
        ## assign i locally to getinverse value of x
        ## x here is the list of functions returned by makeCacheMatrix
        ## if the inverse has NOT been calculated previously
        ## i is assigned to NULL 
        
        i <- x$getinverse()
        
        ## check if inverse has been cached. it is null if it has
        ## not been cached
        if(!is.null(i)) {
                message("getting cached data")
                return(i)
        }
        
        ## data is the returned value from calling x$get
        ## calling x$get returns x as defined in makeCacheMatrix
        ## remember that x is the arg that was passed 
        ## to makeCacheMatrix originally
        
        data <- x$get()
        
        ## calling solve will re-assign i locally because it uses 
        ## the single arrow operator. 
        ## this means that within cacheSolve, i goes from 
        ## its initialized value of NULL to the inverse of 
        ## the matrix passed to makeCacheMatrix
        
        i <- solve(data, ...)
        
        
        ## calling x$setinverse and passing i which is the inverse 
        ## of x reassigns i in the parent environment 
        
        ## now the initial value of i in makeCacheMatrix is the inverse of x 
        ## rather than NULL
        ## when cacheSolve is called a second time and passed the 
        ## same matrix it will call x$getinverse which 
        ## will return a value rather than NULL
        
        ## cache
        x$setinverse(i)
        
        i
}
