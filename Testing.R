j <- function(x) {
  y <- 2
  function() {
    c(x, y)
  }
}

k <- j(1)
k()


testFun <- function(resp, pred, ...) {
  args <- list(resp, pred, ...)
  metrics <- args[toupper(args) %in% c("RMSE", "MAE")]
  args <- args |>
    setdiff(metrics)
  
  return(args)
}


getMetrics_test <- function(resp, pred, ...) {
  
  # validate inputs
  if(!(is.vector(resp) & is.vector(pred))){
    stop('arguments must both be vectors')
  } else if(length(resp) != length(pred)) {
    stop('vectors must be of same length')
  } else if (!(is.atomic(resp) & is.atomic(pred))) {
    stop('vectors must be atomic')
  } else if (!(is.numeric(resp) & is.numeric(pred))) {
    stop('vectors must be numeric')
  }
  
  # store all arguments, including the unnamed arguments
  args <- list(resp, pred, ...)
  
  # create metrics subset list (contains RMSE, MAE)
  metrics <- args[toupper(args) %in% c("RMSE", "MAE")]
  
  # delete the metrics from the args list, leaving just the vectors and unnamed args
  args <- args |>
    setdiff(metrics)
  
  # preserve metrics as names (to apply later to the results list)
  names(metrics) <- unlist(metrics)
  
  # add "get" to the front of the metric that was passed in (made uppercase)
  metrics <- map(metrics, \(x) paste0("get", toupper(x)))
  
  # create empty list for results
  results <- list()
  
  # each element in the list is now a function - use do.call to call the function by a variable
  for (i in 1:length(metrics)) {
    results[i] <- do.call(metrics[[i]], args)
    names(results)[i] <- names(metrics)[i]
  }
  
  # return results
  return(results)
}

test_url <- "https://newsapi.org/v2/everything?q=autoimmune%20diseases&from=2024-10-01&page=1&sortBy=popularity&apiKey=96a47f770685492faaf437063d733d14"

# actual results: 411
# my results: capped at 100?

info <- GET(test_url)

test_news_info <- fromJSON(rawToChar(info$content)) |>
  pluck("articles")


df1 <- data.frame(a = c(1, 2, 3), b = c(0.1, 0.2, 0.3))
df2 <- data.frame(a = c(4, 5, 6), b = c(0.4, 0.5, 0.6))

df_new <- rbind(df1, df2)

row.names(df1)
