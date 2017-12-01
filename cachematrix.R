## Functions to cache the inverse of a matrix
## first function is to create a list containing:
##      1) setting the matrix
##      2) getting the matrix
##      3) setting the inverse of the matrix
##      4) getting the inverse of the matrix


makeCacheMatrix <- function(x = matrix()) {
        # x as a square invertible matrix
        
        inv<- NULL 
        set.mat<- function(y){
                x<<-y
                inv<<-NULL
                
        }
        
        get.mat<-function(){
                x
        }
        set.inv<-function(inverse) {
                inv <<- inverse
        }
        get.inv<-function(){
                inv
        }
        
        ## creation of the function list
        list (set.mat=set.mat,
              get.mat=get.mat,
              set.inv=set.inv,
              get.inv=get.inv
              )
}


## second function gets the inverse of the matrix.
## first checks if the inverse has already been calculated which then skips the calculation.
## otherwise it will calculate the inverse of the function using solve()
## function assumes that the matrix is always invertible


cacheSolve <- function(x, ...) {
        
        # x as output from prev function
        # sets inv as the matrix's inverse
        inv <- x$get.inv()
        
        #checks if inverse is already calculated, returns the cached value
        if(!is.null(inv)){
              message("getting cached inverse data")
                return(inv)
  
        }
        #solves the inverse of the matrix 
        mat.data<-x$get.mat()
        inv = solve(mat.data,...)
        
        #caches the inverse
        x$set.inv(inv)
        
        inv
        
}
