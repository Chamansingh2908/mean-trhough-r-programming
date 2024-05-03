# mean-trhough-r-programming
# Create a function that caches computations
make_cacheable <- function(func) {
  cache <- NULL
  
  # Define a function that checks the cache before computing
  cached_function <- function(...) {
    args <- list(...)

    # Convert arguments to a unique identifier for caching
    args_str <- paste0(args, collapse = "_")
    
    if (is.null(cache) || !identical(cache$args_str, args_str)) {
      # If the cache is empty or the arguments have changed, compute and cache the result
      result <- func(...)
      cache <<- list(args_str = args_str, result = result)
    } else {
      # If the arguments are the same, retrieve the result from the cache
      result <- cache$result
    }
    
    return(result)
  }
  
  return(cached_function)
}

# Example usage: Create a cacheable version of mean function
cached_mean <- make_cacheable(mean)

# Compute mean of a vector, caching the result
vector <- c(1, 2, 3, 4, 5)
mean1 <- cached_mean(vector)
print(mean1)  # Output: 3

# Compute mean of the same vector again, retrieving the result from cache
mean2 <- cached_mean(vector)
print(mean2)  # Output: 3

# Modify the vector and compute mean again, recomputing and caching the result
vector <- c(1, 2, 3, 4, 5, 6)
mean3 <- cached_mean(vector)
print(mean3)  # Output: 3.5
