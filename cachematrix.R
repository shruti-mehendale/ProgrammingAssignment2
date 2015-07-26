## The goal of this assignment is to write a set of functions that will 
## take a matrix as input, compute its inverse, stores these two matrices
## and retrive this stored data when asked again. If the input matrix is reset
## the inverse will be computed and reset.
## Here, function makeCacheMatrix is used to store the matrix and its inverse
## and to reset the matrix and hence the inverse matrix. Function Cachesolve
## either computes the inverse or retrives the already compute inverse.

##
## makeCacheMatrix: takes a matrix as input and produces a list of 4 functions. 
## get() and getInv() store the input matrix and its inverse respectively.
## Invrse matrix is set to NULL initially.
## cacheSolve function takes the matrix and inverse from get() and getInv() 
## from the list. Invesrse is computed there and passed to setInv() in the 
## list which will store it as matrix I. 
## If the same inverse matrix is required again, it can be fetched from 
## getInv() in the list without computing again.
## if one wants to reset the input matrix then, set() function will reset the 
## input matrix and bring inverse matrix I to NULL.
##  
## 
makeCacheMatrix <- function(x = matrix()) {
        
        I <- NULL
        set <- function(y= matrix()) {
               x <<- y
                I <<- NULL
        }
        get <- function()x
        setInv <- function(inv) I <<- inv
        getInv <- function() I
        list(set= set, get= get, 
             setInv = setInv,
             getInv = getInv)
}

#############################################################################

## cacheSolve: returns the inverse of the matrix 'x'.
## first it checks if inverse is previously stored in the list.   
## if yes, retrives it. otherwise, computes inverse and 
## pasees it back to the list.   

cacheSolve<- function(x,...){

        
        I <- x$getInv()
                if(!is.null(I)) {
                       message("Getting cached data")
                       return(I)}
        
## I is the matrix which was previously stored with getInv().
## It is checked wheather the matrix I exsists and it is not empty. 
## If it is not empty, then the previously stored matrix I is returned.    
      
                else{ 
                       newM <- x$get()
                       inv <- solve(newM)
                       x$setInv(inv)
                       return(inv)
                     }
        }
## if I is empty it means input matrix is reset. Inverse will be computed.
## newM : the new matrix (which getMatrix() fetches from makeCachematrix) 
## inv : Inverse of the input matrix
###########################################################################3
