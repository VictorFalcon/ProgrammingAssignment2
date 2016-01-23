#Purpose:avoid repeated computations of a matrix inverse when a previous calculation was done.
#the goal of the  2 functions is to receive a squared matrix,check for existing inverse;
#if no inverse is found,compute the inverse;
#store it in a cache for future usage; display it.
# We'll test our two function with a example matrix

# Part1 
#The function receive input of  a matrix.We'll define 4 functions inside 
#the function MakeCacheMatrix(),and store them in a list. In short,this function
# is a list of 4 functions that act on the inputed matrix: set,get the matrix,
#set,get the inverse. We'll use the terms "give a value" for set and "grab a value" 
#for get in the comments.We'll use the word"subset" as way to explain but not necessarily
# in the same exact sens as subsetting a dataframe...(well; may a bit of contradiction , cause a
#dataframe could be view as list)

makeCacheMatrix <- function(x = matrix()) {
        
        inv <- NULL 
        set <- function (y){  
                x <<-y # value of y being assigned to x is searched through the 
                        #the parent environment
               inv <<-NULL   
             }

get <- function()x # grabs matrix x defined in the main function  MakeCacheMatrix
setinverse <- function(inverse) inv <<- inverse # gives a value to the  inverse
getinverse <- function() inv #grabs the value in inv
list(set=set,get=get,setinverse=setinverse,getinverse=getinverse) # the list of the 4 functions
}
#Part2
#1.Test the double condition ( if inverse matrix was previously stored)
#and (if the matrix has not changed) to return the inverse matrix m
#or compute m

cacheSolve <- function(x,...) {
        
inv<-x$getinverse() # we are subsetting a function the our list and 
                        #applying it to the matrix/we are grabing the inverse and 
                        #assigning it to inv

if(!is.null(inv)){ # if inv is not null, i.e previously calcuted,it simply returns it
                message("getting cached inverse")
                return(inv)  # return is a primitive function used to print inv
                             # it does the same thing as ( x$setinverse ; inv )        
        }
if(is.null(inv)){    # if inv is not null,the follow steps will be used to calculate it.
        data <-x$get() # we subsetthe function get() from the list and applyit to x/we grab x, our data.
        inv<- solve(data,...) # function solve() in r, returns the inverse
        x$setinverse  # here we are subsetting a function for our list to store inv
        
                inv # the value in inv is printed 
            }
}

#the following is not part of the assignement but a test 
# the  2 codelines test : the makeCacheMatrix(),and the outcome "test" is passed
# to cacheSolve();the returned inv =matrix(c(2,-3,-1,2),2,2)
test <- makeCacheMatrix(x=matrix(c(2,3,1,2),ncol=2, nrow=2))
cacheSolve(test)

