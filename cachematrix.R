
###################################################################
# Peer-graded Assignment: Programming Assignment 2: Lexical Scoping
# Coded in "RStudio Version 3.5.1" 
# 13/08/2018
##################################################################


# "makeCacheMatrix" returns a list of fuctions for storing, retrieving, setting the inverse
# matrix and finally retrieving the previously stored inverse matrix 

makeCacheMatrix <- function (m = matrix ())
{
   matrixInverse <- NULL               # this variable will be used as the memory space for storing the inverse matrix
        
   set <- function (setMatrix){        # this is the setter function, it stores "setMatrix" matrix in memory
      m <<- setMatrix
      matrixInverse <<- NULL           # obviously a new matrix might create a different inverse, so the pre-
                                       #  viously stored inverse matrix must be removed.
    }
        
   get <- function () m                # the get function simply returns the matrix 
        
   setInverse <- function (inverse) {  # this function sets a matrix as the inverse matrix, I coded this way because 
                                       # I followed the sample code in the assignment text. I think it is not a good 
                                       # idea let the user set any arbitary matrix as the inverse of a matrix. the in-
                                       #  verse matrix must be calculated automatically and correctly through the
                                       # code itself.
            matrixInverse <<- inverse
   }
        
   getInverse <- function () {         # this function returns the stored inverse matrix
      matrixInverse
   }
   
   list (set = set, get = get, setInverse = setInverse, getInverse = getInverse) # the list of functions
}


# "cacheSolve" retrieves the previously stored inverse matrix. If there is none, it generates an inverse
# matrix, stores it and finally returns it.

cacheSolve <- function (fList, ...){  
   cachedInverse <-  fList$getInverse()                  # retrieving the stored inverse matrix from cache
   
   if (length(cachedInverse) > 0){                       # checking if the cache is not empty
      message ("Fetching inverse matrix from cache ...");   
      return (cachedInverse);                            # returning the cache value and exiting the function
   }
   
   m <- fList$get()                                      # retrieving the matrix for computing the inverse matrix
   
   if (nrow(m) == ncol(m)){                              # check to make sure the matrix is square
      mInverse <- solve (m, ...)                         # generating the inverse matrix using the "solve" function
      fList$setInverse (mInverse)                        # storing the computed inverse matrix in cache 
      mInverse                                           # returning the generated inverse matrix
   }
   else{
      message("The matrix is not square !!!")            # no inverse matrix for non-square matrixes!!! In reality we 
                                                         # must check other conditions, but I just checked this easy 
                                                         # condition :)
   }
}
