# The PersistenceFisherKernel class works

    Code
      krnl$apply(dgm, dgm)
    Output
      [1] 1

---

    Code
      krnl$fit(list(dgm))$transform(list(dgm))
    Output
           [,1]
      [1,]    1

---

    Code
      krnl$fit_transform(list(dgm))
    Output
           [,1]
      [1,]    1

# The PersistenceScaleSpaceKernel class works

    Code
      krnl$apply(dgm, dgm)
    Output
      [1] 0.2933229

---

    Code
      krnl$fit(list(dgm))$transform(list(dgm))
    Output
                [,1]
      [1,] 0.2933229

---

    Code
      krnl$fit_transform(list(dgm))
    Output
                [,1]
      [1,] 0.2933229

# The PersistenceWeightedGaussianKernel class works

    Code
      krnl$apply(dgm, dgm)
    Output
      [1] 32.31432

---

    Code
      krnl$fit(list(dgm))$transform(list(dgm))
    Output
               [,1]
      [1,] 32.31432

---

    Code
      krnl$fit_transform(list(dgm))
    Output
               [,1]
      [1,] 32.31432

# The PersistenceSlicedWassersteinKernel class works

    Code
      krnl$apply(dgm, dgm)
    Output
      [1] 1

---

    Code
      krnl$fit(list(dgm))$transform(list(dgm))
    Output
           [,1]
      [1,]    1

---

    Code
      krnl$fit_transform(list(dgm))
    Output
           [,1]
      [1,]    1

