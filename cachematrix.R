#The 2 functions recive & create a squared matrix,
#check for existing inverse;if no inverse is found,compute the inverse;
#store it in a cache for future usage.

# Part1 
#1.This functions create a matrix by receiving input
#2.This functions test is the inverse mathmatically defined?

makeCacheMatrix <- function(x = matrix()) {
        
        m <- NULL 
        set <- function (y){
                x<-matrix()
                x <<-y
                m <<-NULL   
                
        }
        # mean should be read as inverse and mean() be replaced by solve()
        #as it is a of matter just store and return values/m is the inverse
        get <- function()x
        setmean <- function(mean) m <<- mean
        getmean <- function() m
        list(set=set,get=get,setmean=setmean,getmean=getmean)
        
}
#Part2
#1.Test the double condition ( if inverse matrix was previously stored)
#and (if the matrix has not changed) to return the inverse matrix m
#or compute m

cacheSolve <- function(x, ...) {
        
        m<-x$getsolve()
        if((!is.null(m)) & (x==y)){
                message("getting cached inverse")
                return(m)
        }
        if((is.null(m)) & (x==y)){    
                data <-x$get()
                m <- solve(data,...)
                x$setsovlve(m)
                m
                
        }
}
