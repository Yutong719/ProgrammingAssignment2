## cache the inverse of a matrix

## This is a funciton to create a special matrix, which is really a list containing functions to 
## set/get the value of a matrix
## set/get the value of the inverse of a matrix

makeCacheMatrix <- function(x = matrix()) {
        m<-NULL
        set<-function(y){
                x<<-y
                m<<-NULL
        }
        get<-function()x
        setinverse<-function(inverse) m<<-inverse
        getinverse<-function() m
        list(set=set,get=get,setinverse=setinverse,getinverse=getinverse)
}


## the following function gets the inverse of the special matrix created with the above function

cacheSolve <- function(x, ...) {
        m<-x$getinverse()
        if(!is.null(m)){
                message("getting cached data")
                return(m)
        }
        data<-x$get()
        m<-solve(data,...)
        x$setinverse(m)
        m
        ## Return a matrix that is the inverse of 'x'
}
