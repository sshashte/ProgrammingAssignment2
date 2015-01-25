

# as its name set, makeCacheMatrix function only does a cache of 
#a matrix and returns a list with the elements needed to set a 
# new cache or read the actual cache
cache <- NULL
flag <- NULL
makeCacheMatrix <- function( x = matrix() ){  
  
  j <- NULL
  clearcache <- function(x){ 
  k <- sqrt(length(cache))
  l <- sqrt(length(x)) 
  if( k == l)j <- x %*% cache 
  d <- diag(j)
  if(k != sum(d)) cache<<-NULL
  }
  setcache <- function(y) cache<<- y
  getcache <- function()  cache
  
  ls = list(setcache = setcache, getcache = getcache, clearcache=clearcache)
  cache
  return (ls)
}

# cacheSolve function look into makeCacheMatrix for any cached data
# if there are not cahed data, calls to caching an inverse of the
# matrix it receives as argument

cacheSolve <- function(x, ...) {
 ls <- makeCacheMatrix()
 ls$clearcache(x)
 INV <-ls$getcache()
  if(!is.null(INV) && !is.na(INV)) {
    message("getting cached data")
  }
  if(is.null(INV)) {message("empty cache data, setting inverse")
  ls$setcache(solve(x))
  INV <-ls$getcache()
  }
  #INV
  return (INV)
}
