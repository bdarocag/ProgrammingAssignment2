## El siguiente script, almacena en caché la inversa de una matriz

## makeCacheMatrix construye un objeto de tipo matriz

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinv <- function(solve) m <<- solve
  getinv <- function() m
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)

}


## CacheSolve calcula la inversa de la matriz devuelta por la funcion anterior

cacheSolve <- function(x, ...) {
        ## Devuelve una matriz que es la inversa de 'x'
  m <- x$getinv()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinv(m)
  m
}
