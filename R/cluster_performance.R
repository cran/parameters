#' Performance of clustering models
#'
#' Compute performance indices for clustering solutions.
#'
#' @inheritParams model_parameters.hclust
#'
#' @examples
#' # kmeans
#' model <- kmeans(iris[1:4], 3)
#' cluster_performance(model)
#'
#' # hclust
#' data <- iris[1:4]
#' model <- hclust(dist(data))
#' clusters <- cutree(model, 3)
#' cluster_performance(model, data, clusters)
#'
#' # Retrieve performance from parameters
#' params <- model_parameters(kmeans(iris[1:4], 3))
#' cluster_performance(params)
#' @export
cluster_performance <- function(model, ...) {
  UseMethod("cluster_performance")
}


#' @export
cluster_performance.kmeans <- function(model, ...) {
  out <- as.data.frame(model[c("totss", "betweenss", "tot.withinss")])
  colnames(out) <- c("Sum_Squares_Total", "Sum_Squares_Between", "Sum_Squares_Within")

  out$R2 <- out$Sum_Squares_Between / out$Sum_Squares_Total

  row.names(out) <- NULL
  class(out) <- c("performance_model", class(out))

  out
}


#' @rdname cluster_performance
#' @export
cluster_performance.hclust <- function(model, data, clusters, ...) {
  if (is.null(data)) {
    insight::format_error(
      "This function requires the data used to compute the clustering to be provided via `data` as it is not accessible from the clustering object itself."
    )
  }
  if (is.null(clusters)) {
    insight::format_error(
      "This function requires a vector of clusters assignments of same length as data to be passed, as it is not contained in the clustering object itself."
    )
  }

  params <- model_parameters(model, data = data, clusters = clusters, ...)

  cluster_performance(params)
}


#' @export
cluster_performance.dbscan <- function(model, data, ...) {
  if (is.null(data)) {
    insight::format_error(
      "This function requires the data used to compute the clustering to be provided via `data` as it is not accessible from the clustering object itself."
    )
  }

  params <- model_parameters(model, data = data, ...)

  cluster_performance(params)
}


# Base --------------------------------------------------------------------


#' @export
cluster_performance.parameters_clusters <- function(model, ...) {
  valid <- model$Cluster != 0 & model$Cluster != "0" # Valid clusters
  out <- data.frame(
    Sum_Squares_Total = attributes(model)$Sum_Squares_Total,
    Sum_Squares_Between = attributes(model)$Sum_Squares_Between,
    Sum_Squares_Within = sum(model$Sum_Squares[valid], na.rm = TRUE)
  )

  out$R2 <- out$Sum_Squares_Between / out$Sum_Squares_Total

  class(out) <- c("performance_model", class(out))

  out
}
