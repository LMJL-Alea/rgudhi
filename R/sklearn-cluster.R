#' Base Class for Clustering Algorithms
#'
#' This is the base class for all the clustering algorithms in the
#' [**sklearn.cluster**](https://scikit-learn.org/stable/modules/classes.html#module-sklearn.cluster)
#' module. The child classes are intended to be used within some GUDHI classes
#' such as [Atol].
#'
#' @keywords internal
ClusteringAlgorithm <- R6::R6Class(
  classname = "ClusteringAlgorithm",
  inherit = PythonClass
)

#' Affinity Propagation Clustering
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
  inherit = ClusteringAlgorithm,
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
    #' @examples
    #' if (reticulate::py_module_available("sklearn.cluster")) {
    #'   cl <- AffinityPropagation$new()
    #' }
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

#' Agglomerative Clustering
#'
#' @description Recursively merges pair of clusters of sample data; uses linkage
#'   distance. This is a wrapper around the Python class
#'   [sklearn.cluster.AgglomerativeClustering](https://scikit-learn.org/stable/modules/generated/sklearn.cluster.AgglomerativeClustering.html#sklearn.cluster.AgglomerativeClustering).
#'
#' @export
AgglomerativeClustering <- R6::R6Class(
  classname = "AgglomerativeClustering",
  inherit = ClusteringAlgorithm,
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
    #' @examples
    #' if (reticulate::py_module_available("sklearn.cluster")) {
    #'   cl <- AgglomerativeClustering$new()
    #' }
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

#' Birch Clustering
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
  inherit = ClusteringAlgorithm,
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
    #'   [ClusteringAlgorithm] specifying the number of clusters after the final
    #'   clustering step, which treats the subclusters from the leaves as new
    #'   samples.
    #'   - `NULL`: the final clustering step is not performed and the
    #'   subclusters are returned as they are;
    #'   - an object of class [ClusteringAlgorithm]: the model is fit treating
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
    #' @examples
    #' if (reticulate::py_module_available("sklearn.cluster")) {
    #'   cl <- Birch$new()
    #' }
    initialize = function(threshold = 0.5,
                          branching_factor = 50L,
                          n_clusters = 3L,
                          compute_labels = TRUE,
                          copy = TRUE) {
      branching_factor <- as.integer(branching_factor)
      if (!is.null(n_clusters)) {
        if ("ClusteringAlgorithm" %in% class(n_clusters))
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

#' DBSCAN Clustering
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
  inherit = ClusteringAlgorithm,
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
    #' @examples
    #' if (reticulate::py_module_available("sklearn.cluster")) {
    #'   cl <- DBSCAN$new()
    #' }
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

#' Feature Agglomeration Clustering
#'
#' @description
#' Recursively merges pair of clusters of features. This is a wrapper around the
#' Python class
#' [sklearn.cluster.FeatureAgglomeration](https://scikit-learn.org/stable/modules/generated/sklearn.cluster.FeatureAgglomeration.html#sklearn.cluster.FeatureAgglomeration).
#'
#' @export
FeatureAgglomeration <- R6::R6Class(
  classname = "FeatureAgglomeration",
  inherit = ClusteringAlgorithm,
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
    #' @examples
    #' if (reticulate::py_module_available("sklearn.cluster")) {
    #'   cl <- FeatureAgglomeration$new()
    #' }
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

#' K-Means Clustering
#'
#' @description
#' This is a wrapper around the Python class
#' [sklearn.cluster.KMeans](https://scikit-learn.org/stable/modules/generated/sklearn.cluster.KMeans.html#sklearn.cluster.KMeans).
#'
#' @export
KMeans <- R6::R6Class(
  classname = "KMeans",
  inherit = ClusteringAlgorithm,
  public = list(
    #' @description The [KMeans] class constructor.
    #'
    #' @param n_clusters An integer value specifying the number of clusters to
    #'   form as well as the number of centroids to generate. Defaults to `2L`.
    #' @param init Either a string or a numeric matrix of shape
    #'   \eqn{\mathrm{n_clusters} \times \mathrm{n_features}} specifying the
    #'   method for initialization. If a string, choices are:
    #'   - `"k-means++"`: selects initial cluster centroids using sampling based
    #'   on an empirical probability distribution of the points’ contribution to
    #'   the overall inertia. This technique speeds up convergence, and is
    #'   theoretically proven to be \eqn{\mathcal{O}(\log ⁡k})-optimal. See the
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
    #'
    #' @return An object of class [KMeans].
    #'
    #' @examples
    #' if (reticulate::py_module_available("sklearn.cluster")) {
    #'   cl <- KMeans$new()
    #' }
    initialize = function(n_clusters = 2L,
                          init = c("k-means++", "random"),
                          n_init = 10L,
                          max_iter = 300L,
                          tol = 1e-4,
                          verbose = 0L,
                          random_state = NULL,
                          copy_x = TRUE,
                          algorithm = c("lloyd", "elkan")) {
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

#' Bisecting K-Means Clustering
#'
#' @description
#' This is a wrapper around the Python class
#' [sklearn.cluster.BisectingKMeans](https://scikit-learn.org/stable/modules/generated/sklearn.cluster.BisectingKMeans.html#sklearn.cluster.BisectingKMeans).
#'
#' @export
BisectingKMeans <- R6::R6Class(
  classname = "BisectingKMeans",
  inherit = ClusteringAlgorithm,
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
    #'   theoretically proven to be \eqn{\mathcal{O}(\log ⁡k})-optimal. See the
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
    #' @examples
    #' if (reticulate::py_module_available("sklearn.cluster")) {
    #'   cl <- BisectingKMeans$new()
    #' }
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

#' Mini-Batch K-Means Clustering
#'
#' @description
#' This is a wrapper around the Python class
#' [sklearn.cluster.MiniBatchKMeans](https://scikit-learn.org/stable/modules/generated/sklearn.cluster.MiniBatchKMeans.html#sklearn.cluster.MiniBatchKMeans).
#'
#' @export
MiniBatchKMeans <- R6::R6Class(
  classname = "MiniBatchKMeans",
  inherit = ClusteringAlgorithm,
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
    #'   theoretically proven to be \eqn{\mathcal{O}(\log ⁡k})-optimal. See the
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
    #' @examples
    #' if (reticulate::py_module_available("sklearn.cluster")) {
    #'   cl <- MiniBatchKMeans$new()
    #' }
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
