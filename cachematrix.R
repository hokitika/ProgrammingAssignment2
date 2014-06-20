## File description     :This file contains 2 functions
##                       - makeCacheMatrix()
##                       - cacheSolve()
##                       to create a special object to hold a matrix and and a  
##                       cache for its inverse as well as a function to calculate the inverse.
##
## Usage example 
## m<-matrix(c(4,3,3,2), 2, 2)
## my_matrix<-makeCacheMatrix(m)
## cacheSolve(my_matrix) 

             
## Name                  :makeCacheMatrix()
## Input Parameter       :a square matrix of which an inverse can be calculated 
## Return value          :a list of functions (set, get, setinverse, getinverse)
## Description           :create a special type of matrix-object which 
##                        has getter/setter for the original matrix and its cache (the inverse)

makeCacheMatrix <- function(x = matrix()) {
        #initialise the cache to be a matrix
        cache <- matrix()
        
        #setter for matrix
        set <- function(y= matrix()) {
                x <<- y
                #clean cache
                cache <<- matrix()
        }
        
        #getter for matrix
        get <- function() x
        
        #setter for the cache 
        setinverse <- function(inverse = matrix()) cache <<- inverse
        
        #getter for the cache
        getinverse <- function() cache
        
        #returns a list of functions (defined above)
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}

## Name                  :solveCache()
## Input Parameter       :a special matrix created with makeCacheMatrix() 
## Return value          :the inverse of the matrix
## Description           :calculate the inverse of a matrix by using solve() 
##                        and update the special matrix cache with the inverse

cacheSolve <- function(x, ...) {
        #Get the inverse of the matrix from the cache of the special matrix object 
        i <- x$getinverse()
        
        #Check if the cache can be used
        if(!is.na(i[1,1])) {
                #Issue a message to state that cached inverse of matrix is returned 
                message("getting cached data")
                #Return cached inverese of matrix and exit function 
                return(i)
        }
        
        #Inverse needs to be calculated ...
        
        #Initialise a variable with the matrix
        data <- x$get()
        
        #Calculate the inverse using solve()
        i <- solve(data, ...)
        
        #Update the cache of the special object to hold the inverse of the matrix
        x$setinverse(i)
        
        #return the inverse
        i
}

## EOF


