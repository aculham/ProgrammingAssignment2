
### cachematrix.R
### R Programming - Assignment 2
### August 21, 2014


## function takes a matrix as input and constructs the 
## necessary get/set methods for the matrix and its inverse
makeCacheMatrix = function( mat = matrix() )
{
    inv = NULL      # initialize the inverse
    
    # set the matrix, and set the inverse to NULL
    set.mat = function( mat2 )
    {
        mat <<- mat2
        inv <<- NULL
    }
    
    # get the matrix
    get.mat = function() mat
    
    # set the inverse (computed eslewhere)
    set.inv = function( inv2 ) inv <<- inv2
    
    # get the inverse
    get.inv = function() inv

    list( set.mat=set.mat, get.mat=get.mat,
          set.inv=set.inv, get.inv=get.inv )
}

## function takes an object of the above type
## and computes/stores its inverse
cacheSolve = function( cacheMatrix, ... )
{
    # get the inverse
    inv = cacheMatrix$get.inv()
    
    # if it is not null, return it - no work to do
    if( !is.null( inv ) )
    {
        message( "using cached inverse" )
        return( inv )
    }
    
    # if the inverse was null, compute it now
    
    # get the matrix
    mat = cacheMatrix$get.mat()
    
    # compute the inverse
    inv = solve( mat )
    
    # store it so we can use it later
    cacheMatrix$set.inv( inv )
    
    return( inv )
}