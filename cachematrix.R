## The makeCacheMatrix function constructs a list which contains a 
## matrix and some methods which can be applied to it: set(to set a new matrix),
## get (to return the matrix), setinverse(to set the inverse of the function.
## note that this does not compute a value, just caches a value given to it, which 
## may be wrong), getinverse(to return the inverted matrix)
## The cacheSolve function returns the cached inverse of the matrix if it exists,
## otherwise it inverts the matrix, sets the inverted matrix in the cached matrix object,
## and returns the newly cached inverse



## Constructs a matrix and caches its inverse

makeCacheMatrix <- function(x = matrix()) {
        inv<-NULL         #initialization (empty constructor)
        
        set<-function(y)        # a constructor which gives (or replaces) matrix value,
                                # and erases the inverted value (if it exists)
        {
                x<<-y
                inv<<-NULL
        }
        
        get<-function()         # returns the matrix
        {                       #looks better with brackets, though unnecessary
                x
        }
        
        setinverse<-function(solve)
        {
                inv<<-solve
        }
        
        getinverse<-function()
        {
                inv
        }
        
        list(set=set,get=get,setinverse=setinverse,getinverse=getinverse) #the list returned to the main program
                

}


## Returns the inverse of a cacheMatrix object, if not available inverts and caches the inverse

cacheSolve <- function(x, ...) {
        
        inv<-x$getinverse()
        if(!is.null(inv)) #if there's a cached version there's no meed to re-invert
        {
                message("Getting cached data")
                return(inv)
        }
        ## the next bit gets accessed if there wasn't a chched inverse 
        ##(so the function hasn't returned anything, and is still going strong )
        
        data<-x$get()
        inv<-solve(data)
        x$setinverse(inv)
        inv

        ## Return a matrix that is the inverse of 'x'
}
