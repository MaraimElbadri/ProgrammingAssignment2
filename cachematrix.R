##' @details R programming Assignment 2
##' @details created on 25/01/2015



## This script consists of two functions:
##
## The first function, `makeCacheMatrix()` creates a special "matrix", which is
##   a list containing 4 functions to
##
## 1.  set the value of the matrix
## 2.  get the value of the matrix
## 3.  set the value of the inverse
## 4.  get the value of the inverse

## The second function, 'cacheSolve()' calculates the inverse of the invertible matrix
## created with the first function. Before calculating the inverse, using the
## solve() function, the function checks to see if the inverse has already been 
## calculated. If so, it `get`s the inverse  from the cache and skips the computation. 
## and return existing value stored in makeCasheMatrix(). In other words, the execution
## enviroment of makeCacheMatrix become the enclosing enviroment of cacheSolve().
## Otherwise, if the inverse has not been computed, the R will go to the parent enviroment of
## makeCacheMatrix() to define given variables and calculates the the inverse of the matrix and
## sets the value of the inverse in the cache via the 'setinverse()' function.


## ------------------------------------------------------------------------------------ 
## Usage:
## m <- matrix(c(2,4,3,1,5,7,8,3,2), nrow=3, ncol=3,byrow=TRUE)
## inverse.methods <- makeCacheMatrix(NULL)  #Creating a makeCacheMatrix object 
## str(inverse.methods)  #List of 4 functions
## inverse.methods$set(m)   #Changing the value of the matrix associated with the object
## value <- inverse.methods$get() 
## cacheSolve(inverse.methods) 
## NOTE: you might need to run "cacheSolve(inverse.methods)" multiple times with the
## same matrix and different ones. While doing so, notice the performance of the calculation
## in terms of time. You will notice that the performing times slightly drops, in parts
## of seconds when cacheSolve() called on the same matrix. In this case the cached inverse is 
## retrieved instead of recomputed which is faster.
## --------------------------------------------------------------------------------------


##' @name makeCacheMatrix - makeCacheMatrix: This function creates a special "matrix" 
##' object that can cache its inverse.
##'@param x is an invertible matrix.
##'@details set() function is to change the value of the matrix on which to calculate the inverse 
##'without having to reassign the function to the variable 
makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        
        ##' This function will receive a matrix and assign it to x in the parent 
        ##' environment via the super assignment operator <<-
        set <- function(m){
                x <<- m
                inv <<- NULL
        }
        
        ##' @param none
        ##' @return x, a matrix which will be set by the "set()" function
        ##' @details this function will return x from its parent environment
        get <- function() x
        
        
        setinverse <- function(inverse) inv <<- inverse
        
        ##' @param none
        ##' @return the inverse of the matrix
        getinverse <- function()inv
        
        #list of the functions that created upon the call of makeCacheMatrix function
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
                
}


##' @name cacheSolve function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
##' If the inverse has already been calculated (and the matrix has not changed), 
##' then the cachesolve should retrieve the inverse from the cache.
##' @param x, an object list of 4 functions 
##' @return inv, the inverse of the the matrix. The matrix can be accessed through x and get() method
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        
        start_time <- Sys.time()
        
        ## the local variable 'inv' - different to the inv in makeCacheMatrix function
        # is assigned to the value of the inverse using the "getinverse()" method of 
        # the makeCacheMatrix()
        inv <-x$getinverse()
        
        ## conditional statement to evaluate if the inverse of the matrix has already been
        ## calculated before and cashed.If so, the inverse of the matrix will be 
        ## returned. If the evaluation is FALSE, what is inside teh curly brackets will 
        ## be ignored.
        if(!is.null(inv)){
                message("Getting the cached inverse of the matrix")
                
                ## System.time() values used to measure how long a function will take to retrieve
                ## or recompute an inverse of a matrix
                end_time <- Sys.time()
                time_duration <- end_time - start_time
                print(time_duration)
                
                ## return inverse value if it's already cached.
                return(inv)
        }
        
        ## Assigning the matrix, which accessed through x object and the get(), 
        ## to the 'matrix' variable.
        matrixx <- x$get()
        
        ## calculate the inverse of the matrix by calling the build-in R function
        ## solve() and pass the matrix variable.
        inv <-solve(matrixx, ...)
        
        ## This is the method for cashing, cashing the value of the inverse and pass 
        ## it to the setinverse() function. Recall, the setinverse() function has an
        ## assignment to inv <<- inverse. By passing the inv to setinverse(), it will be 
        ## assigned to the "inv" variable in the parent enviroment of makeCacheMatrix()
        ## At the beganning that variable has a NULL value and now being populated. So at
        ## this point no calculation of the inverse is made in cacheinverse function.
        ## only the storage and retrieval of both the matrix and of the inverse.
        ## 
        x$setinverse(inv)
        
        ## Do some time measurements
        end_time <- Sys.time()
        time_duration <- end_time - start_time
        print(time_duration)
        
        ## return the value of the inverse 
        inv
}
