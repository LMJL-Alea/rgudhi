# rgudhi 0.2.0

In this minor release, I:

* Added the `persistence_diagram_sample` class;
* Implemented the Lagragian formulation to compute the Wasserstein barycenters 
of a sample of persistence diagrams;
* Fixed CRAN warnings:

    * Incoherence of `autoplot` method implementation following renaming of main 
argument `x` into `object`.
    * Rectify an invalid URL pointing to the paper for BIRCH clustering.

# rgudhi 0.1.0

**rgudhi** `v0.1.0` provides an almost full wrapper of the `v3.7.1` of the GUDHI 
library for topological data analysis. Only the cover complex class is missing 
due to non-reproducibility issues with random number generators. With GUDHI 
accessible from R, **rgudhi** `v0.1.0` features:

- data structure to encode simplicial complexes;
- computation of persistence diagrams;
- various usual preprocessing tools for persistence diagrams;
- a dedicated `S3` class `persistence_diagram` for persistence diagram;
- `plot()` and `ggplot2::autoplot()` methods for `persistence_diagram` objects;
- vector and kernel representations of persistence diagrams;
- a number of metrics to quantify distances between persistence diagrams 
(Bottleneck, Persistence Fisher, Wasserstein, Slice-Wasserstein).
- functions to sample points from sphere (`sphere()`) and torus (`torus()`);
- a persistence-based clustering algorithm coined *Tomato*.

The package also wraps all clustering algorithms from the **sklearn.cluster**
module because they can be useful when using the `Atol` vectorization method for
persistence diagram.

It also wraps all scalers classes from **sklearn.preprocessing** for use in
various classes as well.
