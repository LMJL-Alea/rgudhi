#' Base Class for Clustering Algorithms
#'
#' This is the base class for all the clustering algorithms in the
#' [**sklearn.cluster**](https://scikit-learn.org/stable/modules/classes.html#module-sklearn.cluster)
#' module. The child classes are intended to be used within some GUDHI classes
#' such as [`Atol`].
#'
#' @keywords internal
BaseClustering <- R6::R6Class(
  classname = "BaseClustering",
  inherit = SKLearnClass
)

#' Performs clustering according to the affinity propagation algorithm
#'
#' @description This is a wrapper around the Python class
#'   [sklearn.cluster.AffinityPropagation](https://scikit-learn.org/stable/modules/generated/sklearn.cluster.AffinityPropagation.html#sklearn.cluster.AffinityPropagation).
#'
#' @references
#' - Brendan J. Frey and Delbert Dueck (2007). *Clustering by Passing Messages
#' Between Data Points*, Science.
#'
#' @export
AffinityPropagation <- R6::R6Class(
  classname = "AffinityPropagation",
  inherit = BaseClustering,
  public = list(
    #' @description The [AffinityPropagation] class constructor.
    #'
    #' @param damping A numeric value specifying the damping factor in the range
    #'   \eqn{[0.5, 1.0)} which is the extent to which the current value is
    #'   maintained relative to incoming values (weighted `1 - damping`). This
    #'   avoids numerical oscillations when updating these values (messages).
    #'   Defaults to `0.5`.
    #' @param max_iter An integer value specifying the maximum number of
    #'   iterations. Defaults to `200L`.
    #' @param convergence_iter An integer value specifying the number of
    #'   iterations with no change in the number of estimated clusters that
    #'   stops the convergence. Defaults to `15L`.
    #' @param copy A boolean value specifying whether to make a copy of input
    #'   data. Defaults to `TRUE`.
    #' @param preference A numeric value or numeric vector specifying the
    #'   preferences for each point. Points with larger values of preferences
    #'   are more likely to be chosen as exemplars. The number of exemplars,
    #'   i.e. of clusters, is influenced by the input preferences value. If the
    #'   preferences are not passed as arguments, they will be set to the median
    #'   of the input similarities. Defaults to `NULL`.
    #' @param affinity A string specifying the affinity to use. At the moment
    #'   `"precomputed"` and `"euclidean"` are supported. `"euclidean"` uses the
    #'   negative squared euclidean distance between points. Defaults to
    #'   `"euclidean"`.
    #' @param verbose A boolean value specifying whether to be verbose. Defaults
    #'   to `FALSE`.
    #' @param random_state An integer value specifying the seed of the random
    #'   generator. Defaults to `NULL` which uses current time. Set it to a
    #'   fixed integer for reproducible results across function calls.
    #'
    #' @return An object of class [AffinityPropagation].
    #'
    #' @examplesIf reticulate::py_module_available("sklearn.cluster")
    #' cl <- AffinityPropagation$new()
    initialize = function(damping = 0.5,
                          max_iter = 200L,
                          convergence_iter = 15L,
                          copy = TRUE,
                          preference = NULL,
                          affinity = c("euclidean", "precomputed"),
                          verbose = FALSE,
                          random_state = NULL) {
      affinity <- rlang::arg_match(affinity)
      if (!is.null(random_state)) random_state <- as.integer(random_state)
      super$set_python_class(
        skl_cluster$AffinityPropagation(
          damping = damping,
          max_iter = as.integer(max_iter),
          convergence_iter = as.integer(convergence_iter),
          copy = copy,
          preference = preference,
          affinity = affinity,
          verbose = verbose,
          random_state = random_state
        )
      )
    }
  )
)

#' Performs clustering according to the agglomerative algorithm
#'
#' @description Recursively merges pair of clusters of sample data; uses linkage
#'   distance. This is a wrapper around the Python class
#'   [sklearn.cluster.AgglomerativeClustering](https://scikit-learn.org/stable/modules/generated/sklearn.cluster.AgglomerativeClustering.html#sklearn.cluster.AgglomerativeClustering).
#'
#' @export
AgglomerativeClustering <- R6::R6Class(
  classname = "AgglomerativeClustering",
  inherit = BaseClustering,
  public = list(
    #' @description The [AgglomerativeClustering] class constructor.
    #'
    #' @param n_clusters An integer value specifying the number of clusters to
    #'   find. It must be `NULL` if `distance_threshold` is not `NULL`. Defaults
    #'   to `2L`.
    #' @param affinity A string specifying the metric used to compute the
    #'   linkage. Can be `"euclidean"`, `"l1"`, `"l2"`, `"manhattan"`,
    #'   `"cosine"` or `"precomputed"`. If `linkage` is `"ward"`, only
    #'   `"euclidean"` is accepted. If `"precomputed"`, a distance matrix
    #'   (instead of a similarity matrix) is needed as input for the `$fit()`
    #'   method. Defaults to `"euclidean"`.
    #' @param memory A string specifying the path to the caching directory.
    #'   Defaults to `NULL` in which case no caching is done.
    #' @param connectivity Either a numeric matrix or an object of class
    #'   [stats::dist] or an object coercible into a function by
    #'   [rlang::as_function()] specifying for each sample the neighboring
    #'   samples following a given structure of the data. This can be a
    #'   connectivity matrix itself or a function that transforms the data into
    #'   a connectivity matrix. Defaults to `NULL`, i.e., the hierarchical
    #'   clustering algorithm is unstructured.
    #' @param compute_full_tree Either a boolean value or the `"auto"` string
    #'   specifying whether to prematurely stop the construction of the tree at
    #'   `n_clusters`. This is useful to decrease computation time if the number
    #'   of clusters is not small compared to the number of samples. This option
    #'   is useful only when specifying a connectivity matrix. Note also that
    #'   when varying the number of clusters and using caching, it may be
    #'   advantageous to compute the full tree. It must be `TRUE` if
    #'   `distance_threshold` is not `NULL`. Defaults to `"auto"`, which is
    #'   equivalent to `TRUE` when `distance_threshold` is not `NULL` or that
    #'   `n_clusters` is inferior to the maximum between `100` and `0.02 *
    #'   n_samples`. Otherwise, `"auto"` is equivalent to `FALSE`.
    #' @param linkage A string specifying which linkage criterion to use. The
    #'   linkage criterion determines which distance to use between sets of
    #'   observation. The algorithm will merge the pairs of cluster that
    #'   minimize this criterion.
    #'   - `ward`: minimizes the variance of the clusters being merged;
    #'   - `average`: uses the average of the distances of each observation of
    #'   the two sets;
    #'   - `complete`: uses the maximum of the distances between all
    #'   observations of the two sets.
    #'   - `single`: uses the minimum of the distances between all observations
    #'   of the two sets.
    #'   Defaults to `"ward"`.
    #' @param distance_threshold A numeric value specifying the linkage distance
    #'   threshold above which clusters will not be merged. If not `NULL`,
    #'   `n_clusters` must be `NULL` and `compute_full_tree` must be `TRUE`.
    #'   Defaults to `NULL`.
    #' @param compute_distances A boolean value specifying whether to compute
    #'   distances between clusters even if `distance_threshold` is not used.
    #'   This can be used to make dendrogram visualization, but introduces a
    #'   computational and memory overhead. Defaults to `FALSE`.
    #'
    #' @return An object of class [AgglomerativeClustering].
    #'
    #' @examplesIf reticulate::py_module_available("sklearn.cluster")
    #' cl <- AgglomerativeClustering$new()
    initialize = function(n_clusters = 2L,
                          affinity = c("euclidean", "l1", "l2", "manhattan",
                                       "cosine", "precomputed"),
                          memory = NULL,
                          connectivity = NULL,
                          compute_full_tree = "auto",
                          linkage = c("ward", "complete", "average", "single"),
                          distance_threshold = NULL,
                          compute_distances = FALSE) {
      n_clusters <- as.integer(n_clusters)
      affinity <- rlang::arg_match(affinity)
      if (!is.null(connectivity)) {
        connectivity <- if (is.matrix(connectivity))
          connectivity
        else if (inherits(connectivity, "dist"))
          as.matrix(connectivity)
        else
          rlang::as_function(connectivity)
      }

      linkage <- rlang::arg_match(linkage)
      super$set_python_class(
        skl_cluster$AgglomerativeClustering(
          n_clusters = n_clusters,
          affinity = affinity,
          memory = memory,
          connectivity = connectivity,
          compute_full_tree = compute_full_tree,
          linkage = linkage,
          distance_threshold = distance_threshold,
          compute_distances = compute_distances
        )
      )
    }
  )
)

#' Performs clustering according to the Birch algorithm
#'
#' @description
#' It is a memory-efficient, online-learning algorithm provided as an
#' alternative to [MiniBatchKMeans]. It constructs a tree data structure with
#' the cluster centroids being read off the leaf. These can be either the final
#' cluster centroids or can be provided as input to another clustering algorithm
#' such as [AgglomerativeClustering]. This is a wrapper around the Python class
#'   [sklearn.cluster.Birch](https://scikit-learn.org/stable/modules/generated/sklearn.cluster.Birch.html#sklearn.cluster.Birch).
#'
#' @references
#' - Tian Zhang, Raghu Ramakrishnan, Maron Livny (1996). *BIRCH: An efficient
#' data clustering method for large databases*,
#' <https://www.cs.sfu.ca/CourseCentral/459/han/papers/zhang96.pdf>.
#' - Roberto Perdisci J. *Birch - Java implementation of BIRCH clustering
#' algorithm*, <https://code.google.com/archive/p/jbirch>.
#'
#' @export
Birch <- R6::R6Class(
  classname = "Birch",
  inherit = BaseClustering,
  public = list(
    #' @description The [Birch] class constructor.
    #'
    #' @param threshold A numeric value specifying the upper bound of the radius
    #'   of the subcluster obtained by merging a new sample and the closest
    #'   subcluster. Otherwise a new subcluster is started. Setting this value
    #'   to be very low promotes splitting and vice-versa. Defaults to `0.5`.
    #' @param branching_factor An integer value specifying the maximum number of
    #'   CF subclusters in each node. If a new sample enters such that the
    #'   number of subclusters exceeds the `branching_factor` then that node is
    #'   splitted into two nodes with the subclusters redistributed in each. The
    #'   parent subcluster of that node is removed and two new subclusters are
    #'   added as parents of the 2 split nodes.
    #' @param n_clusters Either an integer value or an object of class
    #'   [BaseClustering] specifying the number of clusters after the final
    #'   clustering step, which treats the subclusters from the leaves as new
    #'   samples.
    #'   - `NULL`: the final clustering step is not performed and the
    #'   subclusters are returned as they are;
    #'   - an object of class [BaseClustering]: the model is fit treating
    #'   the subclusters as new samples and the initial data is mapped to the
    #'   label of the closest subcluster;
    #'   - integer value: the model fit is [AgglomerativeClustering] with
    #'   `n_clusters` set to be equal to the integer value.
    #' Defaults to `3L`.
    #' @param compute_labels A boolean value specifying whether to compute
    #'   labels for each fit. Defaults to `TRUE`.
    #' @param copy A boolean value specifying whether to make a copy of the
    #'   given data. If set to `FALSE`, the initial data will be overwritten.
    #'   Defaults to `TRUE`.
    #'
    #' @return An object of class [Birch].
    #'
    #' @examplesIf reticulate::py_module_available("sklearn.cluster")
    #' cl <- Birch$new()
    initialize = function(threshold = 0.5,
                          branching_factor = 50L,
                          n_clusters = 3L,
                          compute_labels = TRUE,
                          copy = TRUE) {
      branching_factor <- as.integer(branching_factor)
      if (!is.null(n_clusters)) {
        if ("BaseClustering" %in% class(n_clusters))
          n_clusters <- n_clusters$get_python_class()
        else
          n_clusters <- as.integer(n_clusters)
      }
      super$set_python_class(
        skl_cluster$Birch(
          threshold = threshold,
          branching_factor = branching_factor,
          n_clusters = n_clusters,
          compute_labels = compute_labels,
          copy = copy
        )
      )
    }
  )
)

#' Performs clustering according to the DBSCAN algorithm
#'
#' @description
#' DBSCAN - Density-Based Spatial Clustering of Applications with Noise. Finds core samples of high density and expands clusters from them. Good for data which contains clusters of similar density. This is a wrapper around the Python class
#'   [sklearn.cluster.DBSCAN](https://scikit-learn.org/stable/modules/generated/sklearn.cluster.DBSCAN.html#sklearn.cluster.DBSCAN).
#'
#' @references
#' - Ester, M., H. P. Kriegel, J. Sander, and X. Xu (1996). *A Density-Based
#' Algorithm for Discovering Clusters in Large Spatial Databases with Noise*,
#' In: Proceedings of the 2nd International Conference on Knowledge Discovery
#' and Data Mining, Portland, OR, AAAI Press, pp. 226-231.
#' - Schubert, E., Sander, J., Ester, M., Kriegel, H. P., & Xu, X. (2017).
#' *DBSCAN revisited, revisited: why and how you should (still) use DBSCAN*, ACM
#' Transactions on Database Systems (TODS), **42**(3), p. 19.
#'
#' @export
DBSCAN <- R6::R6Class(
  classname = "DBSCAN",
  inherit = BaseClustering,
  public = list(
    #' @description The [DBSCAN] class constructor.
    #'
    #' @param eps A numeric value specifying the maximum distance between two
    #'   samples for one to be considered as in the neighborhood of the other.
    #'   This is not a maximum bound on the distances of points within a
    #'   cluster. This is the most important DBSCAN parameter to choose
    #'   appropriately for your data set and distance function. Defaults to
    #'   `0.5`.
    #' @param min_samples An integer value specifying the number of samples (or
    #'   total weight) in a neighborhood for a point to be considered as a core
    #'   point. This includes the point itself. Defaults to `5L`.
    #' @param metric Either a string or an object coercible into a function via
    #'   [rlang::as_function()] specifying the metric to use when calculating
    #'   distance between instances in a feature array. If `metric` is a string,
    #'   it must be one of the options allowed by
    #'   [sklearn.metrics.pairwise_distances](https://scikit-learn.org/stable/modules/generated/sklearn.metrics.pairwise_distances.html#sklearn.metrics.pairwise_distances)
    #'   for its `metric` parameter. If `metric` is `"precomputed"`, `X` is
    #'   assumed to be a distance matrix and must be square. `X` may be a sparse
    #'   graph, in which case only *nonzero* elements may be considered
    #'   neighbors for DBSCAN. Defaults to `"euclidean"`.
    #' @param metric_params A named list specifying additional parameters to be
    #'   passed on to the metric function. Defaults to `NULL`.
    #' @param algorithm A string specifying the algorithm to be used by the
    #'   [sklearn.neighbors.NearestNeighbors](https://scikit-learn.org/stable/modules/classes.html#module-sklearn.neighbors)
    #'    module to compute pointwise distances and find nearest neighbors.
    #'   Choices are `"auto"`, `"ball_tree"`, `"kd_tree"` or `"brute"`. Defaults
    #'   to `"auto"`.
    #' @param leaf_size An integer value specifying the leaf size passed to
    #'   [sklearn.neighbors.BallTree](https://scikit-learn.org/stable/modules/generated/sklearn.neighbors.BallTree.html)
    #'    or
    #'   [sklearn.neighbors.KDTree](https://scikit-learn.org/stable/modules/generated/sklearn.neighbors.KDTree.html#sklearn.neighbors.KDTree).
    #'   This can affect the speed of the construction and query, as well as the
    #'   memory required to store the tree. The optimal value depends on the
    #'   nature of the problem. Defaults to `30L`.
    #' @param p An integer value specifying the power of the Minkowski metric to
    #'   be used to calculate distance between points. Defaults to `2L`.
    #' @param n_jobs An integer value specifying the number of parallel jobs to
    #'   run. Defaults to `1L`.
    #'
    #' @return An object of class [DBSCAN].
    #'
    #' @examplesIf reticulate::py_module_available("sklearn.cluster")
    #' cl <- DBSCAN$new()
    initialize = function(eps = 0.5,
                          min_samples = 5L,
                          metric = "euclidean",
                          metric_params = NULL,
                          algorithm = c("auto", "ball_tree", "kd_tree", "brute"),
                          leaf_size = 30L,
                          p = 2L,
                          n_jobs = 1L) {
      min_samples <- as.integer(min_samples)
      if (!is.character(metric)) metric <- rlang::as_function(metric)
      if (!is.null(metric_params) && !rlang::is_named(metric_params))
        cli::cli_abort("When provided, the {.arg metric_params} must be a named
                       list.")
      algorithm <- rlang::arg_match(algorithm)
      leaf_size <- as.integer(leaf_size)
      p <- as.integer(p)
      n_jobs <- as.integer(n_jobs)
      super$set_python_class(
        skl_cluster$DBSCAN(
          eps = eps,
          min_samples = min_samples,
          metric = metric,
          metric_params = metric_params,
          algorithm = algorithm,
          leaf_size = leaf_size,
          p = p,
          n_jobs = n_jobs
        )
      )
    }
  )
)

#' Performs clustering according to the feature agglomeration algorithm
#'
#' @description
#' Recursively merges pair of clusters of features. This is a wrapper around the
#' Python class
#' [sklearn.cluster.FeatureAgglomeration](https://scikit-learn.org/stable/modules/generated/sklearn.cluster.FeatureAgglomeration.html#sklearn.cluster.FeatureAgglomeration).
#'
#' @export
FeatureAgglomeration <- R6::R6Class(
  classname = "FeatureAgglomeration",
  inherit = BaseClustering,
  public = list(
    #' @description The [FeatureAgglomeration] class constructor.
    #'
    #' @param n_clusters An integer value specifying the number of clusters to
    #'   find. Defaults to `2L`.
    #' @param affinity A string or an object coercible into a function via
    #'   [rlang::as_function()] specifying the metric used to compute the
    #'   linkage. If a string, choices are `"euclidean"`, `"l1"`, `"l2"`,
    #'   `"manhattan"`, `"cosine"` or `"precomputed"`. If linkage is `"ward"`,
    #'   only `"euclidean"` is accepted. Defaults to `"euclidean"`.
    #' @param memory A string specifying path to the caching directory for
    #'   storing the computation of the tree. Defaults to `NULL` in which case
    #'   no caching is done.
    #' @param connectivity A numeric matrix or an object coercible into a
    #'   function via [rlang::as_function()] specifying the connectivity matrix.
    #'   Defines for each feature the neighboring features following a given
    #'   structure of the data. This can be a connectivity matrix itself or a
    #'   function that transforms the data into a connectivity matrix, such as
    #'   derived from
    #'   [sklearn.neighbors.kneighbors_graph()](https://scikit-learn.org/stable/modules/generated/sklearn.neighbors.kneighbors_graph.html#sklearn.neighbors.kneighbors_graph).
    #'   Defaults to `NULL` in which case the hierarchical clustering algorithm
    #'   is unstructured.
    #' @param compute_full_tree The string `"auto"` or a boolean value
    #'   specifying whether to stop early the construction of the tree at
    #'   `n_clusters`. This is useful to decrease computation time if the number
    #'   of clusters is not small compared to the number of features. This
    #'   option is useful only when specifying a connectivity matrix. Note also
    #'   that when varying the number of clusters and using caching, it may be
    #'   advantageous to compute the full tree. It must be `TRUE` if
    #'   `distance_threshold` is not `NULL`. Defaults to `"auto"`, which is
    #'   equivalent to `TRUE` when `distance_threshold` is not `NULL` or when
    #'   `n_clusters` is inferior to `max(100, 0.02 * n_samples)` and to `FALSE`
    #'   otherwise.
    #' @param linkage A string specifying which linkage criterion to use. The
    #'   linkage criterion determines which distance to use between sets of
    #'   features. The algorithm will merge the pairs of cluster that minimize
    #'   this criterion:
    #'   - `"ward"`: minimizes the variance of the clusters being merged;
    #'   - `"complete"`: maximum linkage uses the maximum distances between all
    #'   features of the two sets;
    #'   - `"average"`: uses the average of the distances of each feature of the
    #'   two sets;
    #'   - `"single"`: uses the minimum of the distances between all features of
    #'   the two sets.
    #' @param pooling_func An object coercible into a function via
    #'   [rlang::as_function()] specifying the aggregation method to combine the
    #'   values of agglomerated features into a single value. It should take as
    #'   input an array of shape \eqn{M \times N} and the optional argument
    #'   `axis = 1`, and reduce it to an array of shape \eqn{M}. Defaults to
    #'   [base::rowMeans].
    #' @param distance_threshold A numeric value specifying the linkage distance
    #'   threshold above which clusters will not be merged. If not `NULL`,
    #'   `n_clusters` must be `NULL` and `compute_full_tree` must be `TRUE`.
    #'   Defaults to `NULL`.
    #' @param compute_distances A boolean value specifying whether to compute
    #'   distances between clusters even if `distance_threshold` is not used.
    #'   This can be used to make dendrogram visualization, but introduces a
    #'   computational and memory overhead. Defaults to `FALSE`.
    #'
    #' @return An object of class [FeatureAgglomeration].
    #'
    #' @examplesIf reticulate::py_module_available("sklearn.cluster")
    #' cl <- FeatureAgglomeration$new()
    initialize = function(n_clusters = 2L,
                          affinity = c("euclidean", "l1", "l2", "manhattan",
                                       "cosine", "precomputed"),
                          memory = NULL,
                          connectivity = NULL,
                          compute_full_tree = "auto",
                          linkage = c("ward", "complete", "average", "single"),
                          pooling_func = rowMeans ,
                          distance_threshold = NULL,
                          compute_distances = FALSE) {
      if (!is.null(distance_threshold))
        n_clusters <- NULL
      else
        n_clusters <- as.integer(n_clusters)
      if (is.character(affinity))
        affinity <- rlang::arg_match(affinity)
      else
        affinity <- rlang::as_function(affinity)
      if (!is.null(connectivity))
        if (!is.matrix(connectivity))
          connectivity <- rlang::as_function(connectivity)
      linkage <- rlang::arg_match(linkage)
      super$set_python_class(
        skl_cluster$FeatureAgglomeration(
          n_clusters = n_clusters,
          affinity = affinity,
          memory = memory,
          connectivity = connectivity,
          compute_full_tree = compute_full_tree,
          linkage = linkage,
          pooling_func = pooling_func,
          distance_threshold = distance_threshold,
          compute_distances = compute_distances
        )
      )
    }
  )
)

#' Performs clustering according to the k-means algorithm
#'
#' @description
#' This is a wrapper around the Python class
#' [sklearn.cluster.KMeans](https://scikit-learn.org/stable/modules/generated/sklearn.cluster.KMeans.html#sklearn.cluster.KMeans).
#'
#' @export
KMeans <- R6::R6Class(
  classname = "KMeans",
  inherit = BaseClustering,
  public = list(
    #' @description The [KMeans] class constructor.
    #'
    #' @param n_clusters An integer value specifying the number of clusters to
    #'   form as well as the number of centroids to generate. Defaults to `2L`.
    #' @param init Either a string or a numeric matrix of shape
    #'   \eqn{\mathrm{n_{clusters}} \times \mathrm{n_{features}}} specifying the
    #'   method for initialization. If a string, choices are:
    #'   - `"k-means++"`: selects initial cluster centroids using sampling based
    #'   on an empirical probability distribution of the points’ contribution to
    #'   the overall inertia. This technique speeds up convergence, and is
    #'   theoretically proven to be \eqn{\mathcal{O}(\log(k))}-optimal. See the
    #'   description of `n_init` for more details;
    #'   - `"random"`: chooses `n_clusters` observations (rows) at random from
    #'   data for the initial centroids.
    #'
    #'   Defaults to `"k-means++"`.
    #' @param n_init An integer value specifying the number of times the k-means
    #'   algorithm will be run with different centroid seeds. The final results
    #'   will be the best output of `n_init` consecutive runs in terms of
    #'   inertia. Defaults to `10L`.
    #' @param max_iter An integer value specifying the maximum number of
    #'   iterations of the k-means algorithm for a single run. Defaults to
    #'   `300L`.
    #' @param tol A numeric value specifying the relative tolerance with regards
    #'   to Frobenius norm of the difference in the cluster centers of two
    #'   consecutive iterations to declare convergence. Defaults to `1e-4`.
    #' @param verbose An integer value specifying the level of verbosity.
    #'   Defaults to `0L` which is equivalent to no verbose.
    #' @param random_state An integer value specifying the initial seed of the
    #'   random number generator. Defaults to `NULL` which uses the current
    #'   timestamp.
    #' @param copy_x A boolean value specifying whether the original data is to
    #'   be modified. When pre-computing distances it is more numerically
    #'   accurate to center the data first. If `copy_x` is `TRUE`, then the
    #'   original data is not modified. If `copy_x` is `FALSE`, the original
    #'   data is modified, and put back before the function returns, but small
    #'   numerical differences may be introduced by subtracting and then adding
    #'   the data mean. Note that if the original data is not C-contiguous, a
    #'   copy will be made even if `copy_x` is `FALSE`. If the original data is
    #'   sparse, but not in CSR format, a copy will be made even if `copy_x` is
    #'   `FALSE`. Defaults to `TRUE`.
    #' @param algorithm A string specifying the k-means algorithm to use. The
    #'   classical EM-style algorithm is `"lloyd"`. The `"elkan"` variation can
    #'   be more efficient on some datasets with well-defined clusters, by using
    #'   the triangle inequality. However it’s more memory-intensive due to the
    #'   allocation of an extra array of shape \eqn{\mathrm{n_{samples}} \times
    #'   \mathrm{n_{clusters}}}. Defaults to `"lloyd"`.
    #'
    #' @return An object of class [KMeans].
    #'
    #' @examplesIf reticulate::py_module_available("sklearn.cluster")
    #' cl <- KMeans$new()
    initialize = function(n_clusters = 2L,
                          init = c("k-means++", "random"),
                          n_init = 10L,
                          max_iter = 300L,
                          tol = 1e-4,
                          verbose = 0L,
                          random_state = NULL,
                          copy_x = TRUE,
                          algorithm = c("lloyd", "elkan")) {
      n_clusters <- as.integer(n_clusters)
      if (is.character(init))
        init <- rlang::arg_match(init)
      else if (!is.matrix(init) || dim(init)[1] != n_clusters)
        cli::cli_abort('The argument {.arg init} should be either one of
                       {.field "k-means++"} or {.field "random"} or a numeric
                       matrix with {n_clusters} row{?s}.')
      n_init <- as.integer(n_init)
      max_iter <- as.integer(max_iter)
      if (!is.null(random_state))
        random_state <- as.integer(random_state)
      algorithm <- rlang::arg_match(algorithm)
      super$set_python_class(
        skl_cluster$KMeans(
          n_clusters = n_clusters,
          init = init,
          n_init = n_init,
          max_iter = max_iter,
          tol = tol,
          verbose = verbose,
          random_state = random_state,
          copy_x = copy_x,
          algorithm = algorithm
        )
      )
    }
  )
)

#' Performs clustering according to the bisecting k-means algorithm
#'
#' @description
#' This is a wrapper around the Python class
#' [sklearn.cluster.BisectingKMeans](https://scikit-learn.org/stable/modules/generated/sklearn.cluster.BisectingKMeans.html#sklearn.cluster.BisectingKMeans).
#'
#' @export
BisectingKMeans <- R6::R6Class(
  classname = "BisectingKMeans",
  inherit = BaseClustering,
  public = list(
    #' @description The [BisectingKMeans] class constructor.
    #'
    #' @param n_clusters An integer value specifying the number of clusters to
    #'   form as well as the number of centroids to generate. Defaults to `2L`.
    #' @param init Either a string or a numeric matrix of shape
    #'   \eqn{\mathrm{n_clusters} \times \mathrm{n_features}} specifying the
    #'   method for initialization. If a string, choices are:
    #'   - `"k-means++"`: selects initial cluster centroids using sampling based
    #'   on an empirical probability distribution of the points’ contribution to
    #'   the overall inertia. This technique speeds up convergence, and is
    #'   theoretically proven to be \eqn{\mathcal{O}(\log(k))}-optimal. See the
    #'   description of `n_init` for more details;
    #'   - `"random"`: chooses `n_clusters` observations (rows) at random from
    #'   data for the initial centroids.
    #'   Defaults to `"k-means++"`.
    #' @param n_init An integer value specifying the number of times the k-means
    #'   algorithm will be run with different centroid seeds. The final results
    #'   will be the best output of `n_init` consecutive runs in terms of
    #'   inertia. Defaults to `10L`.
    #' @param max_iter An integer value specifying the maximum number of
    #'   iterations of the k-means algorithm for a single run. Defaults to
    #'   `300L`.
    #' @param tol A numeric value specifying the relative tolerance with regards
    #'   to Frobenius norm of the difference in the cluster centers of two
    #'   consecutive iterations to declare convergence. Defaults to `1e-4`.
    #' @param verbose An integer value specifying the level of verbosity.
    #'   Defaults to `0L` which is equivalent to no verbose.
    #' @param random_state An integer value specifying the initial seed of the
    #'   random number generator. Defaults to `NULL` which uses the current
    #'   timestamp.
    #' @param copy_x A boolean value specifying whether the original data is to
    #'   be modified. When pre-computing distances it is more numerically
    #'   accurate to center the data first. If `copy_x` is `TRUE`, then the
    #'   original data is not modified. If `copy_x` is `FALSE`, the original
    #'   data is modified, and put back before the function returns, but small
    #'   numerical differences may be introduced by subtracting and then adding
    #'   the data mean. Note that if the original data is not C-contiguous, a
    #'   copy will be made even if `copy_x` is `FALSE`. If the original data is
    #'   sparse, but not in CSR format, a copy will be made even if `copy_x` is
    #'   `FALSE`. Defaults to `TRUE`.
    #' @param algorithm A string specifying the k-means algorithm to use. The
    #'   classical EM-style algorithm is `"lloyd"`. The `"elkan"` variation can
    #'   be more efficient on some datasets with well-defined clusters, by using
    #'   the triangle inequality. However it’s more memory-intensive due to the
    #'   allocation of an extra array of shape \eqn{\mathrm{n_samples} \times
    #'   \mathrm{n_clusters}}. Defaults to `"lloyd"`.
    #' @param bisecting_strategy A string specifying how bisection should be
    #'   performed. Choices are:
    #'   - `"biggest_inertia"`: means that it will always check all calculated
    #'   cluster for cluster with biggest SSE (Sum of squared errors) and bisect
    #'   it. This approach concentrates on precision, but may be costly in terms
    #'   of execution time (especially for larger amount of data points).
    #'   - `"largest_cluster"`: means that it will always split cluster with
    #'   largest amount of points assigned to it from all clusters previously
    #'   calculated. That should work faster than picking by SSE and may produce
    #'   similar results in most cases.
    #'   Defaults to `"biggest_inertia"`.
    #'
    #' @return An object of class [BisectingKMeans].
    #'
    #' @examplesIf reticulate::py_module_available("sklearn.cluster")
    #' cl <- BisectingKMeans$new()
    initialize = function(n_clusters = 2L,
                          init = c("k-means++", "random"),
                          n_init = 10L,
                          max_iter = 300L,
                          tol = 1e-4,
                          verbose = 0L,
                          random_state = NULL,
                          copy_x = TRUE,
                          algorithm = c("lloyd", "elkan"),
                          bisecting_strategy = c("biggest_inertia",
                                                 "largest_cluster")) {
      if (is.character(init))
        init <- rlang::arg_match(init)
      else if (!is.matrix(init) || dim(init)[1] != n_clusters)
        cli::cli_abort('The argument {.arg init} should be either one of
                       {.field "k-means++"} or {.field "random"} or a numeric
                       matrix with {n_clusters} row{?s}.')
      n_init <- as.integer(n_init)
      if (!is.null(random_state))
        random_state <- as.integer(random_state)
      algorithm <- rlang::arg_match(algorithm)
      bisecting_strategy <- rlang::arg_match(bisecting_strategy)
      super$set_python_class(
        skl_cluster$BisectingKMeans(
          n_clusters = n_clusters,
          init = init,
          n_init = n_init,
          max_iter = max_iter,
          tol = tol,
          verbose = verbose,
          random_state = random_state,
          copy_x = copy_x,
          algorithm = algorithm,
          bisecting_strategy = bisecting_strategy
        )
      )
    }
  )
)

#' Performs clustering according to the mini-batch k-means algorithm
#'
#' @description
#' This is a wrapper around the Python class
#' [sklearn.cluster.MiniBatchKMeans](https://scikit-learn.org/stable/modules/generated/sklearn.cluster.MiniBatchKMeans.html#sklearn.cluster.MiniBatchKMeans).
#'
#' @export
MiniBatchKMeans <- R6::R6Class(
  classname = "MiniBatchKMeans",
  inherit = BaseClustering,
  public = list(
    #' @description The [MiniBatchKMeans] class constructor.
    #'
    #' @param n_clusters An integer value specifying the number of clusters to
    #'   form as well as the number of centroids to generate. Defaults to `2L`.
    #' @param init Either a string or a numeric matrix of shape
    #'   \eqn{\mathrm{n_clusters} \times \mathrm{n_features}} specifying the
    #'   method for initialization. If a string, choices are:
    #'   - `"k-means++"`: selects initial cluster centroids using sampling based
    #'   on an empirical probability distribution of the points’ contribution to
    #'   the overall inertia. This technique speeds up convergence, and is
    #'   theoretically proven to be \eqn{\mathcal{O}(\log(k))}-optimal. See the
    #'   description of `n_init` for more details;
    #'   - `"random"`: chooses `n_clusters` observations (rows) at random from
    #'   data for the initial centroids.
    #'   Defaults to `"k-means++"`.
    #' @param n_init An integer value specifying the number of times the k-means
    #'   algorithm will be run with different centroid seeds. The final results
    #'   will be the best output of `n_init` consecutive runs in terms of
    #'   inertia. Defaults to `10L`.
    #' @param max_iter An integer value specifying the maximum number of
    #'   iterations of the k-means algorithm for a single run. Defaults to
    #'   `300L`.
    #' @param tol A numeric value specifying the relative tolerance with regards
    #'   to Frobenius norm of the difference in the cluster centers of two
    #'   consecutive iterations to declare convergence. Defaults to `1e-4`.
    #' @param verbose An integer value specifying the level of verbosity.
    #'   Defaults to `0L` which is equivalent to no verbose.
    #' @param random_state An integer value specifying the initial seed of the
    #'   random number generator. Defaults to `NULL` which uses the current
    #'   timestamp.
    #' @param batch_size An integer value specifying the size of the
    #'   mini-batches. For faster computations, you can set the `batch_size`
    #'   greater than 256 * number of cores to enable parallelism on all cores.
    #'   Defaults to `1024L`.
    #' @param compute_labels A boolean value specifying whether to compute label
    #'   assignment and inertia for the complete dataset once the minibatch
    #'   optimization has converged in fit. Defaults to `TRUE`.
    #' @param max_no_improvement An integer value specifying how many
    #'   consecutive mini batches that does not yield an improvement on the
    #'   smoothed inertia should be used to call off the algorithm. To disable
    #'   convergence detection based on inertia, set `max_no_improvement` to
    #'   `NULL`. Defaults to `10L`.
    #' @param init_size An integer value specifying the number of samples to
    #'   randomly sample for speeding up the initialization (sometimes at the
    #'   expense of accuracy): the only algorithm is initialized by running a
    #'   batch [KMeans] on a random subset of the data. This needs to be larger
    #'   than `n_clusters`. If `NULL`, the heuristic is `init_size = 3 *
    #'   batch_size` if `3 * batch_size < n_clusters`, else `init_size = 3 *
    #'   n_clusters`. Defaults to `NULL`.
    #' @param reassignment_ratio A numeric value specifying the fraction of the
    #'   maximum number of counts for a center to be reassigned. A higher value
    #'   means that low count centers are more easily reassigned, which means
    #'   that the model will take longer to converge, but should converge in a
    #'   better clustering. However, too high a value may cause convergence
    #'   issues, especially with a small batch size. Defaults to `0.01`.
    #'
    #' @return An object of class [MiniBatchKMeans].
    #'
    #' @examplesIf reticulate::py_module_available("sklearn.cluster")
    #' cl <- MiniBatchKMeans$new()
    initialize = function(n_clusters = 2L,
                          init = c("k-means++", "random"),
                          n_init = 10L,
                          max_iter = 300L,
                          tol = 1e-4,
                          verbose = 0L,
                          random_state = NULL,
                          batch_size = 1024L,
                          compute_labels = TRUE,
                          max_no_improvement = 10L,
                          init_size = NULL,
                          reassignment_ratio = 0.01) {
      if (is.character(init))
        init <- rlang::arg_match(init)
      else if (!is.matrix(init) || dim(init)[1] != n_clusters)
        cli::cli_abort('The argument {.arg init} should be either one of
                       {.field "k-means++"} or {.field "random"} or a numeric
                       matrix with {n_clusters} row{?s}.')
      n_init <- as.integer(n_init)
      if (!is.null(random_state))
        random_state <- as.integer(random_state)
      batch_size <- as.integer(batch_size)
      if (!is.null(init_size)) init_size <- as.integer(init_size)
      super$set_python_class(
        skl_cluster$MiniBatchKMeans(
          n_clusters = n_clusters,
          init = init,
          n_init = n_init,
          max_iter = max_iter,
          tol = tol,
          verbose = verbose,
          random_state = random_state,
          batch_size = batch_size,
          compute_labels = compute_labels,
          max_no_improvement = max_no_improvement,
          init_size = init_size,
          reassignment_ratio = reassignment_ratio
        )
      )
    }
  )
)

#' Performs clustering according to the mean shift algorithm
#'
#' @description
#' This is a wrapper around the Python class
#' [sklearn.cluster.MeanShift](https://scikit-learn.org/stable/modules/generated/sklearn.cluster.MeanShift.html#sklearn.cluster.MeanShift).
#'
#' @export
MeanShift <- R6::R6Class(
  classname = "MeanShift",
  inherit = BaseClustering,
  public = list(
    #' @description The [MeanShift] class constructor.
    #'
    #' @param bandwidth A numeric value specifying the bandwidth used in the RBF
    #'   kernel. If `NULL`, the bandwidth is estimated using
    #'   [sklearn.cluster.estimate_bandwidth()](https://scikit-learn.org/stable/modules/generated/sklearn.cluster.estimate_bandwidth.html).
    #'   Defaults to `NULL`.
    #' @param seeds A numeric matrix of shape \eqn{n_\mathrm{samples} \times
    #'   n_\mathrm{features}} specifying the seeds used to initialize kernels.
    #'   If `NULL`, the seeds are calculated by
    #'   `sklearn.cluster.get_bin_seeds()` with bandwidth as the grid size and
    #'   default values for other parameters. Defaults to `NULL`.
    #' @param bin_seeding A boolean value specifying whether initial kernel
    #'   locations are not locations of all points, but rather the location of
    #'   the discretized version of points, where points are binned onto a grid
    #'   whose coarseness corresponds to the bandwidth. Setting this option to
    #'   `TRUE` will speed up the algorithm because fewer seeds will be
    #'   initialized. Defaults to `FALSE`. Ignored if `seeds` is not `NULL`.
    #' @param min_bin_freq An integer value specifying the minimal size of bins.
    #'   To speed up the algorithm, accept only those bins with at least
    #'   `min_bin_freq` points as seeds. Defaults to `1L`.
    #' @param cluster_all A boolean value specifying whether all points are
    #'   clustered, even those orphans that are not within any kernel. Orphans
    #'   are assigned to the nearest kernel. If `FALSE`, then orphans are given
    #'   cluster label `-1`. Defaults to `TRUE`.
    #' @param n_jobs An integer value specifying the number of jobs to use for
    #'   the computation. This works by computing each of the `n_init` runs in
    #'   parallel. Defaults to `1L`.
    #' @param max_iter An integer value specifying the maximum number of
    #'   iterations per seed point before the clustering operation terminates
    #'   (for that seed point) if it has not yet converged. Defaults to `300L`.
    #'
    #' @return An object of class [MeanShift].
    #'
    #' @examplesIf reticulate::py_module_available("sklearn.cluster")
    #' cl <- MeanShift$new()
    initialize = function(bandwidth = NULL,
                          seeds = NULL,
                          bin_seeding = FALSE,
                          min_bin_freq = 1L,
                          cluster_all = TRUE,
                          n_jobs = 1L,
                          max_iter = 300L) {
      min_bin_freq <- as.integer(min_bin_freq)
      n_jobs <- as.integer(n_jobs)
      max_iter <- as.integer(max_iter)
      super$set_python_class(
        skl_cluster$MeanShift(
          bandwidth = bandwidth,
          seeds = seeds,
          bin_seeding = bin_seeding,
          min_bin_freq = min_bin_freq,
          cluster_all = cluster_all,
          n_jobs = n_jobs,
          max_iter = max_iter
        )
      )
    }
  )
)

#' Performs clustering according to the OPTICS algorithm
#'
#' @description
#' This is a wrapper around the Python class
#' [sklearn.cluster.OPTICS](https://scikit-learn.org/stable/modules/generated/sklearn.cluster.OPTICS.html#sklearn.cluster.OPTICS).
#'
#' @export
OPTICS <- R6::R6Class(
  classname = "OPTICS",
  inherit = BaseClustering,
  public = list(
    #' @description The [OPTICS] class constructor.
    #'
    #' @param min_samples Either an integer value greater than 1 or a numeric
    #'   value between 0 and 1 specifying the number of samples in a
    #'   neighborhood for a point to be considered as a core point. Also, up and
    #'   down steep regions can’t have more than `min_samples` consecutive
    #'   non-steep points. Expressed as an absolute number or a fraction of the
    #'   number of samples (rounded to be at least 2). Defaults to `5L`.
    #' @param max_eps A numeric value specifying the maximum distance between
    #'   two samples for one to be considered as in the neighborhood of the
    #'   other. Reducing `max_eps` will result in shorter run times. Defaults to
    #'   `Inf`.
    #' @param metric Either a string or an object coercible into a function via
    #'   [rlang::as_function()] specifying the metric to use for distance
    #'   computation. If `metric` is a function, it is called on each pair of
    #'   instances (rows) and the resulting value recorded. The function should
    #'   take two numeric vectors as input and return one numeric value
    #'   indicating the distance between them. This works for Scipy’s metrics,
    #'   but is less efficient than passing the metric name as a string. If
    #'   metric is `"precomputed"`, `X` is assumed to be a distance matrix and
    #'   must be square. Valid string values for metric are:
    #'
    #'   - from
    #'   [**sklearn.metrics**](https://scikit-learn.org/stable/modules/classes.html#module-sklearn.metrics):
    #'   `"cityblock"`, `"cosine"`, `"euclidean"`, `"l1"`, `"l2"`,
    #'   `"manhattan"`;
    #'   - from
    #'   [**scipy.spatial.distance**](https://docs.scipy.org/doc/scipy/reference/spatial.distance.html):
    #'   `"braycurtis"`, `"canberra"`, `"chebyshev"`, `"correlation"`, `"dice"`,
    #'   `"hamming"`, `"jaccard"`, `"kulsinski"`, `"mahalanobis"`,
    #'   `"minkowski"`, `"rogerstanimoto"`, `"russellrao"`, `"seuclidean"`,
    #'   `"sokalmichener"`, `"sokalsneath"`, `"sqeuclidean"`, `"yule"`.
    #'
    #'   Defaults to `"minkowski"`.
    #' @param p An integer value specifying the power for the Minkowski metric.
    #'   When `p = 1`, this is equivalent to using the Manhattan distance
    #'   (\eqn{\ell_1}). When `p = 2`, this is equivalent to using the Euclidean
    #'   distance (\eqn{\ell_2}). For arbitrary \eqn{p}, the Minkowski distance
    #'   (\eqn{\ell_p}) is used. Defaults to `2L`.
    #' @param metric_params A named list specifying additional arguments for the
    #'   metric function. Defaults to `NULL`.
    #' @param cluster_method A string specifying the extraction method used to
    #'   extract clusters using the calculated reachability and ordering.
    #'   Possible values are `"xi"` and `"dbscan"`. Defaults to `"xi"`.
    #' @param eps A numeric value specifying the maximum distance between two
    #'   samples for one to be considered as in the neighborhood of the other.
    #'   Defaults to `max_eps`. Used only when `cluster_method == "dbscan"`.
    #' @param xi A numeric value in \eqn{[0,1]} specifying the minimum steepness
    #'   on the reachability plot that constitutes a cluster boundary. For
    #'   example, an upwards point in the reachability plot is defined by the
    #'   ratio from one point to its successor being at most `1 - xi`. Used only
    #'   when `cluster_method == "xi"`. Defaults to `0.05`.
    #' @param predecessor_correction A boolean value specifying whether to
    #'   correct clusters according to the predecessors calculated by OPTICS
    #'   \insertCite{schubert2018improving}{rgudhi}. This parameter has minimal
    #'   effect on most data sets. Used only when `cluster_method == "xi"`.
    #'   Defaults to `TRUE`.
    #' @param min_cluster_size Either an integer value \eqn{> 1} or a numeric
    #'   value in \eqn{[0,1]} specifying the minimum number of samples in an
    #'   OPTICS cluster, expressed as an absolute number or a fraction of the
    #'   number of samples (rounded to be at least 2). If `NULL`, the value of
    #'   `min_samples` is used instead. Used only when `cluster_method == "xi"`.
    #'   Defaults to `NULL`.
    #' @param algorithm A string specifying the algorithm used to compute the
    #'   nearest neighbors. Choices are `c("auto", "ball_tree", "kd_tree",
    #'   "brute")`. Defaults to `"auto"` which will attempt to decide the most
    #'   appropriate algorithm based on the values passed to fit method. Note:
    #'   fitting on sparse input will override the setting of this parameter,
    #'   using `algorithm == "brute"`.
    #' @param leaf_size An integer value specifying the leaf size passed to
    #'   `BallTree` or `KDTree`. This can affect the speed of the construction
    #'   and query, as well as the memory required to store the tree. The
    #'   optimal value depends on the nature of the problem. Defaults to `30L`
    #' @param memory A string specifying the path to the caching directory into
    #'   which caching the output of the computation of the tree. Defaults to
    #'   `NULL` in which case no caching is done.
    #' @param n_jobs An integer value specifying the number of parallel jobs to
    #'   run for neighbors search. Defaults to `1L`. A value of `-1L` means
    #'   using all processors.
    #'
    #' @return An object of class [OPTICS].
    #'
    #' ## References
    #' \insertCited{}
    #'
    #' @examplesIf reticulate::py_module_available("sklearn.cluster")
    #' cl <- OPTICS$new()
    initialize = function(min_samples = 5L,
                          max_eps = Inf,
                          metric = c(
                            "minkowski", "cityblock", "cosine", "euclidean",
                            "l1", "l2", "manhattan", "braycurtis", "canberra",
                            "chebyshev", "correlation", "dice", "hamming",
                            "jaccard", "kulsinski", "mahalanobis",
                            "rogerstanimoto", "russellrao", "seuclidean",
                            "sokalmichener", "sokalsneath", "sqeuclidean",
                            "yule"),
                          p = 2L,
                          metric_params = NULL,
                          cluster_method = c("xi", "dbscan"),
                          eps = NULL,
                          xi = 0.05,
                          predecessor_correction = TRUE,
                          min_cluster_size = NULL,
                          algorithm = c("auto", "ball_tree", "kd_tree", "brute"),
                          leaf_size = 30L,
                          memory = NULL,
                          n_jobs = 1L) {
      if (min_samples > 1) min_samples <- as.integer(min_samples)
      if (is.character(metric))
        metric <- rlang::arg_match(metric)
      else
        metric <- rlang::as_function(metric)
      p <- as.integer(p)
      cluster_method <- rlang::arg_match(cluster_method)
      if (!is.null(min_cluster_size) && min_cluster_size > 1)
        min_cluster_size <- as.integer(min_cluster_size)
      algorithm <- rlang::arg_match(algorithm)
      leaf_size <- as.integer(leaf_size)
      n_jobs <- as.integer(n_jobs)
      super$set_python_class(
        skl_cluster$OPTICS(
          min_samples = min_samples,
          max_eps = max_eps,
          metric = metric,
          p = p,
          metric_params = metric_params,
          cluster_method = cluster_method,
          eps = eps,
          xi = xi,
          predecessor_correction = predecessor_correction,
          min_cluster_size = min_cluster_size,
          algorithm = algorithm,
          leaf_size = leaf_size,
          memory = memory,
          n_jobs = n_jobs
        )
      )
    }
  )
)

#' Performs clustering according to the spectral clustering algorithm
#'
#' @description
#' This is a wrapper around the Python class
#' [sklearn.cluster.SpectralClustering](https://scikit-learn.org/stable/modules/generated/sklearn.cluster.SpectralClustering.html#sklearn.cluster.SpectralClustering).
#'
#' @export
SpectralClustering <- R6::R6Class(
  classname = "SpectralClustering",
  inherit = BaseClustering,
  public = list(
    #' @description The [SpectralClustering] class constructor.
    #'
    #' @param n_clusters An integer value specifying the dimension of the
    #'   projection subspace. Defaults to `8L`.
    #' @param eigen_solver A string specifying the eigenvalue decomposition
    #'   strategy to use. Choices are `c("arpack", "lobpcg", "amg")`. AMG
    #'   requires **pyamg** to be installed. It can be faster on very large,
    #'   sparse problems, but may also lead to instabilities. Defaults to
    #'   `"arpack"`.
    #' @param n_components An integer value specifying the number of
    #'   eigenvectors to use for the spectral embedding. Defaults to `NULL`, in
    #'   which case, `n_clusters` is used.
    #' @param random_state An integer value specifying a pseudo random number
    #'   generator used for the initialization of the `lobpcg` eigenvectors
    #'   decomposition when `eigen_solver == "amg"`, and for the k-means
    #'   initialization. Defaults to `NULL` which uses clock time.
    #' @param n_init An integer value specifying the number of time the k-means
    #'   algorithm will be run with different centroid seeds. The final results
    #'   will be the best output of `n_init` consecutive runs in terms of
    #'   inertia. Only used if `assign_labels == "kmeans"`. Defaults to `10L`.
    #' @param gamma A numeric value specifying the kernel coefficient for `rbf`,
    #'   `poly`, `sigmoid`, `laplacian` and `chi2` kernels. Ignored for
    #'   `affinity == "nearest_neighbors"`. Defaults to `1.0`.
    #' @param affinity Either a string or an object coercible to a function via
    #'   [`rlang::as_function()`] specifying how to construct the affinity
    #'   matrix:
    #'
    #'   - `"nearest_neighbors"`: construct the affinity matrix by computing a
    #'   graph of nearest neighbors;
    #'   - `"rbf"`: construct the affinity matrix using a radial basis function
    #'   (RBF) kernel;
    #'   - `"precomputed"`: interpret `X` as a precomputed affinity matrix,
    #'   where larger values indicate greater similarity between instances;
    #'   - `"precomputed_nearest_neighbors"`: interpret `X` as a sparse graph of
    #'   precomputed distances, and construct a binary affinity matrix from the
    #'   `n_neighbors` nearest neighbors of each instance;
    #'   - one of the kernels supported by
    #'   [pairwise_kernels](https://scikit-learn.org/stable/modules/generated/sklearn.metrics.pairwise.pairwise_kernels.html).
    #'
    #'   Only kernels that produce similarity scores (non-negative values that
    #'   increase with similarity) should be used. This property is not checked
    #'   by the clustering algorithm.
    #'
    #'   Defaults to `"rbf"`.
    #' @param n_neighbors An integer value specifying the number of neighbors to
    #'   use when constructing the affinity matrix using the nearest neighbors
    #'   method. Ignored for `affinity == "rbf"`. Defaults to `10L`.
    #' @param eigen_tol A numeric value specifying the stopping criterion for
    #'   the eigen-decomposition of the Laplacian matrix. If `eigen_tol ==
    #'   "auto"`, then the passed tolerance will depend on the `eigen_solver`:
    #'
    #'   - If `eigen_solver == "arpack"`, then `eigen_tol = 0.0`;
    #'   - If `eigen_solver == "lobpcg"` or `eigen_solver == "amg"`, then
    #'   `eigen_tol == NULL` which configures the underlying `lobpcg` solver to
    #'   automatically resolve the value according to their heuristics.
    #'
    #'   Note that when using `eigen_solver == "lobpcg"` or `eigen_solver ==
    #'   "amg"` values of `tol < 1e-5` may lead to convergence issues and should
    #'   be avoided.
    #'
    #'   Defaults to `"auto"`.
    #' @param assign_labels A string specifying the strategy for assigning
    #'   labels in the embedding space. There are two ways to assign labels
    #'   after the Laplacian embedding. k-means is a popular choice
    #'   (`"kmeans"`), but it can be sensitive to initialization. Discretization
    #'   is another approach which is less sensitive to random initialization
    #'   (`"discretize"`). The `cluster_qr` method directly extract clusters
    #'   from eigenvectors in spectral clustering. In contrast to k-means and
    #'   discretization, `cluster_qr` has no tuning parameters and runs no
    #'   iterations, yet may outperform k-means and discretization in terms of
    #'   both quality and speed. Defaults to `"kmeans"`.
    #' @param degree An integer value specifying the degree of the polynomial
    #'   kernel. Ignored by other kernels. Defaults to `3L`.
    #' @param coef0 A numeric value specifying the value of the zero coefficient
    #'   for polynomial and sigmoid kernels. Ignored by other kernels. Defaults
    #'   to `1.0`.
    #' @param kernel_params A named list specifying extra arguments to the
    #'   kernels passed as functions. Ignored by other kernels. Defaults to
    #'   `NULL`.
    #' @param n_jobs An integer value specifying the number of parallel jobs to
    #'   run for neighbors search. Defaults to `1L`. A value of `-1L` means
    #'   using all processors.
    #' @param verbose A boolean value specifying the verbosity mode. Defaults to
    #'   `FALSE`.
    #'
    #' @return An object of class [SpectralClustering].
    #'
    #' @examplesIf reticulate::py_module_available("sklearn.cluster")
    #' cl <- SpectralClustering$new()
    initialize = function(n_clusters = 8L,
                          eigen_solver = c("arpack", "lobpcg", "amg"),
                          n_components = NULL,
                          random_state = NULL,
                          n_init = 10L,
                          gamma = 1.0,
                          affinity = c("rbf", "nearest_neighbors", "precomputed",
                                       "precomputed_nearest_neighbors"),
                          n_neighbors = 10L,
                          eigen_tol = "auto",
                          assign_labels = c("kmeans", "discretize", "cluster_qr"),
                          degree = 3L,
                          coef0 = 1,
                          kernel_params = NULL,
                          n_jobs = 1L,
                          verbose = FALSE) {
      n_clusters <- as.integer(n_clusters)
      eigen_solver <- rlang::arg_match(eigen_solver)
      if (!is.null(n_components)) n_components <- as.integer(n_components)
      if (!is.null(random_state)) random_state <- as.integer(random_state)
      n_init <- as.integer(n_init)
      if (is.character(affinity))
        affinity <- rlang::arg_match(affinity)
      else
        affinity <- rlang::as_function(affinity)
      n_neighbors <- as.integer(n_neighbors)
      assign_labels <- rlang::arg_match(assign_labels)
      degree <- as.integer(degree)
      if (!is.null(kernel_params)) {
        if (!rlang::is_named(kernel_params))
          cli::cli_abort("The {.arg kernel_params} argument should be either
                         {.code NULL} or a named list.")
      }
      n_jobs <- as.integer(n_jobs)
      super$set_python_class(
        skl_cluster$SpectralClustering(
          n_clusters = n_clusters,
          eigen_solver = eigen_solver,
          n_components = n_components,
          random_state = random_state,
          n_init = n_init,
          gamma = gamma,
          affinity = affinity,
          n_neighbors = n_neighbors,
          eigen_tol = eigen_tol,
          assign_labels = assign_labels,
          degree = degree,
          coef0 = coef0,
          kernel_params = kernel_params,
          n_jobs = n_jobs,
          verbose = verbose
        )
      )
    }
  )
)

#' Performs clustering according to the spectral biclustering algorithm
#'
#' @description
#' This is a wrapper around the Python class
#' [sklearn.cluster.SpectralBiclustering](https://scikit-learn.org/stable/modules/generated/sklearn.cluster.SpectralBiclustering.html#sklearn.cluster.SpectralBiclustering).
#'
#' @export
SpectralBiclustering <- R6::R6Class(
  classname = "SpectralBiclustering",
  inherit = BaseClustering,
  public = list(
    #' @description The [SpectralBiclustering] class constructor.
    #'
    #' @param n_clusters An integer value or a length-2 vector specifying the
    #'   number of row and column clusters in the checkerboard structure.
    #'   Defaults to `3L`.
    #' @param method A string specifying the method of normalizing and
    #'   converting singular vectors into biclusters. May be one of `"scale"`,
    #'   `"bistochastic"` or `"log"`. The authors recommend using `"log"`. If
    #'   the data is sparse, however, log-normalization will not work, which is
    #'   why the default is `"bistochastic"`. Warning: if `method == "log"`, the
    #'   data must not be sparse.
    #' @param n_components An integer value specifying the number of singular
    #'   vectors to check. Defaults to `6L`.
    #' @param n_best An integer value specifying the number of best singular
    #'   vectors to which to project the data for clustering. Defaults to `3L`.
    #' @param svd_method A string specifying the algorithm for finding singular
    #'   vectors. May be `"randomized"` or `"arpack"`. If `"randomized"`, uses
    #'   `randomized_svd()`, which may be faster for large matrices. If
    #'   `"arpack"`, uses `scipy.sparse.linalg.svds()`, which is more accurate,
    #'   but possibly slower in some cases. Defaults to `"randomized"`.
    #' @param n_svd_vecs An integer value specifying the number of vectors to
    #'   use in calculating the SVD. Corresponds to `ncv` when `svd_method ==
    #'   "arpack"` and `n_oversamples` when `svd_method == "randomized"`.
    #'   Defaults to `NULL`.
    #' @param mini_batch A boolean value specifying whether to use mini-batch
    #'   k-means, which is faster but may get different results. Defaults to
    #'   `FALSE`.
    #' @param init A string specifying the method for initialization of k-means
    #'   algorithm. Choices are `"k-means++"` or `"random"`. Defaults to
    #'   `"k-means++"`.
    #' @param n_init An integer value specifying the number of random
    #'   initializations that are tried with the k-means algorithm. If
    #'   mini-batch k-means is used, the best initialization is chosen and the
    #'   algorithm runs once. Otherwise, the algorithm is run for each
    #'   initialization and the best solution chosen. Defaults to `10L`.
    #' @param random_state An integer value specifying a pseudo random number
    #'   generator used for the initialization of the `lobpcg` eigenvectors
    #'   decomposition when `eigen_solver == "amg"`, and for the k-means
    #'   initialization. Defaults to `NULL` which uses clock time.
    #'
    #' @return An object of class [SpectralBiclustering].
    #'
    #' @examplesIf reticulate::py_module_available("sklearn.cluster")
    #' cl <- SpectralBiclustering$new()
    initialize = function(n_clusters = 3L,
                          method = c("bistochastic", "scale", "log"),
                          n_components = 6L,
                          n_best = 3L,
                          svd_method = c("randomized", "arpack"),
                          n_svd_vecs = NULL,
                          mini_batch = FALSE,
                          init = c("k-means++", "random"),
                          n_init = 10L,
                          random_state = NULL) {
      n_clusters <- as.integer(n_clusters)
      if (length(n_clusters) > 1L) {
        if (length(n_clusters) > 2L)
          cli::cli_abort("The argument {.arg n_clusters} should be of length 1 or 2.")
        n_clusters <- reticulate::tuple(n_clusters[1], n_clusters[2])
      }
      method <- rlang::arg_match(method)
      n_components <- as.integer(n_components)
      n_best <- as.integer(n_best)
      svd_method <- rlang::arg_match(svd_method)
      if (!is.null(n_svd_vecs)) n_svd_vecs <- as.integer(n_svd_vecs)
      init <- rlang::arg_match(init)
      n_init <- as.integer(n_init)
      if (!is.null(random_state)) random_state <- as.integer(random_state)

      super$set_python_class(
        skl_cluster$SpectralBiclustering(
          n_clusters = n_clusters,
          method = method,
          n_components = n_components,
          n_best = n_best,
          svd_method = svd_method,
          n_svd_vecs = n_svd_vecs,
          mini_batch = mini_batch,
          init = init,
          n_init = n_init,
          random_state = random_state
        )
      )
    }
  )
)

#' Performs clustering according to the spectral coclustering algorithm
#'
#' @description
#' This is a wrapper around the Python class
#' [sklearn.cluster.SpectralCoclustering](https://scikit-learn.org/stable/modules/generated/sklearn.cluster.SpectralCoclustering.html#sklearn.cluster.SpectralCoclustering).
#'
#' @export
SpectralCoclustering <- R6::R6Class(
  classname = "SpectralCoclustering",
  inherit = BaseClustering,
  public = list(
    #' @description The [SpectralCoclustering] class constructor.
    #'
    #' @param n_clusters An integer value specifying the number of biclusters to
    #'   find. Defaults to `3L`.
    #' @param svd_method A string specifying the algorithm for finding singular
    #'   vectors. May be `"randomized"` or `"arpack"`. If `"randomized"`, uses
    #'   `sklearn.utils.extmath.randomized_svd()`, which may be faster for large
    #'   matrices. If `"arpack"`, uses `scipy.sparse.linalg.svds()`, which is
    #'   more accurate, but possibly slower in some cases. Defaults to
    #'   `"randomized"`.
    #' @param n_svd_vecs An integer value specifying the number of vectors to
    #'   use in calculating the SVD. Corresponds to `ncv` when `svd_method ==
    #'   "arpack"` and `n_oversamples` when `svd_method == "randomized"`.
    #'   Defaults to `NULL`.
    #' @param mini_batch A boolean value specifying whether to use mini-batch
    #'   k-means, which is faster but may get different results. Defaults to
    #'   `FALSE`.
    #' @param init A string specifying the method for initialization of k-means
    #'   algorithm. Choices are `"k-means++"` or `"random"`. Defaults to
    #'   `"k-means++"`.
    #' @param n_init An integer value specifying the number of random
    #'   initializations that are tried with the k-means algorithm. If
    #'   mini-batch k-means is used, the best initialization is chosen and the
    #'   algorithm runs once. Otherwise, the algorithm is run for each
    #'   initialization and the best solution chosen. Defaults to `10L`.
    #' @param random_state An integer value specifying a pseudo random number
    #'   generator used for the initialization of the `lobpcg` eigenvectors
    #'   decomposition when `eigen_solver == "amg"`, and for the k-means
    #'   initialization. Defaults to `NULL` which uses clock time.
    #'
    #' @return An object of class [SpectralCoclustering].
    #'
    #' @examplesIf reticulate::py_module_available("sklearn.cluster")
    #' cl <- SpectralCoclustering$new()
    initialize = function(n_clusters = 3L,
                          svd_method = c("randomized", "arpack"),
                          n_svd_vecs = NULL,
                          mini_batch = FALSE,
                          init = c("k-means++", "random"),
                          n_init = 10L,
                          random_state = NULL) {
      n_clusters <- as.integer(n_clusters)
      svd_method <- rlang::arg_match(svd_method)
      if (!is.null(n_svd_vecs)) n_svd_vecs <- as.integer(n_svd_vecs)
      init <- rlang::arg_match(init)
      n_init <- as.integer(n_init)
      if (!is.null(random_state)) random_state <- as.integer(random_state)

      super$set_python_class(
        skl_cluster$SpectralCoclustering(
          n_clusters = n_clusters,
          svd_method = svd_method,
          n_svd_vecs = n_svd_vecs,
          mini_batch = mini_batch,
          init = init,
          n_init = n_init,
          random_state = random_state
        )
      )
    }
  )
)
