# The BottleneckDistance class works

    Code
      dis$apply(dgm, dgm)
    Output
      [1] 2.20023e-308

---

    Code
      dis$fit(list(dgm))$transform(list(dgm))
    Output
                   [,1]
      [1,] 2.20023e-308

---

    Code
      dis$fit_transform(list(dgm))
    Output
                   [,1]
      [1,] 2.20023e-308

# The PersistenceFisherDistance class works

    Code
      dis$apply(dgm, dgm)
    Output
      [1] 0

---

    Code
      dis$fit(list(dgm))$transform(list(dgm))
    Output
           [,1]
      [1,]    0

---

    Code
      dis$fit_transform(list(dgm))
    Output
           [,1]
      [1,]    0

# The SlicedWassersteinDistance class works

    Code
      dis$apply(dgm, dgm)
    Output
      [1] 0

---

    Code
      dis$fit(list(dgm))$transform(list(dgm))
    Output
           [,1]
      [1,]    0

---

    Code
      dis$fit_transform(list(dgm))
    Output
           [,1]
      [1,]    0

# The WassersteinDistance class works

    Code
      dis$apply(dgm, dgm)
    Output
      [1] 0

---

    Code
      dis$fit(list(dgm))$transform(list(dgm))
    Output
           [,1]
      [1,]    0

---

    Code
      dis$fit_transform(list(dgm))
    Output
           [,1]
      [1,]    0

