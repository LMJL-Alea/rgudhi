test_that("perslay works with Gaussian mixture weight and flat phi", {
  diagrams <- as_persistence_diagram_sample(list(
    as_persistence_diagram(tibble::tibble(
      birth = c(0, 1, 3, 6),
      death = c(4, 2, 8, 8)
    ))
  ))
  ds <- DiagramScaler$new(
    use = TRUE,
    scalers = list(list(c(0, 1), MinMaxScaler$new()))
  )
  dgms <- diagrams |>
    ds$fit_transform() |>
    to_ragged_tensor()

  phi <- FlatPerslayPhi$new(
    samples = c(1:13, 12:1),
    theta = 0.1
  )
  weight <- GaussianMixturePerslayWeight$new(
    gaussians = matrix(c(0, 0, 1, 1), nrow = 4, ncol = 1)
  )
  perm_op <- tf$math$reduce_sum
  rho <- tf$identity

  perslay <- Perslay$new(
    phi = phi,
    weight = weight,
    perm_op = perm_op,
    rho = rho
  )

  vectors <- perslay$call(dgms)
  expect_snapshot(vectors)
})

test_that("perslay works with grid weight and flat phi", {
  diagrams <- as_persistence_diagram_sample(list(
    as_persistence_diagram(tibble::tibble(
      birth = c(0, 1, 3, 6),
      death = c(4, 2, 8, 8)
    ))
  ))
  ds <- DiagramScaler$new(
    use = TRUE,
    scalers = list(list(c(0, 1), MinMaxScaler$new()))
  )
  dgms <- diagrams |>
    ds$fit_transform() |>
    to_ragged_tensor()

  phi <- FlatPerslayPhi$new(
    samples = c(1:13, 12:1),
    theta = 0.1
  )
  weight <- GridPerslayWeight$new(
    grid = matrix(c(1:13, 12:1), 5, 5),
    grid_bnds = rbind(c(-.5, 1.5), c(-.5, 1.5))
  )
  perm_op <- tf$math$reduce_sum
  rho <- tf$identity

  perslay <- Perslay$new(
    phi = phi,
    weight = weight,
    perm_op = perm_op,
    rho = rho
  )

  vectors <- perslay$call(dgms)
  expect_snapshot(vectors)
})

test_that("perslay works with power weight and flat phi", {
  diagrams <- as_persistence_diagram_sample(list(
    as_persistence_diagram(tibble::tibble(
      birth = c(0, 1, 3, 6),
      death = c(4, 2, 8, 8)
    ))
  ))
  ds <- DiagramScaler$new(
    use = TRUE,
    scalers = list(list(c(0, 1), MinMaxScaler$new()))
  )
  dgms <- diagrams |>
    ds$fit_transform() |>
    to_ragged_tensor()

  phi <- FlatPerslayPhi$new(
    samples = c(1:13, 12:1),
    theta = 0.1
  )
  weight <- PowerPerslayWeight$new(
    constant = 1,
    power = 0
  )
  perm_op <- tf$math$reduce_sum
  rho <- tf$identity

  perslay <- Perslay$new(
    phi = phi,
    weight = weight,
    perm_op = perm_op,
    rho = rho
  )

  vectors <- perslay$call(dgms)
  expect_snapshot(vectors)
})


test_that("perslay works with Gaussian mixture weight and Gaussian phi", {
  diagrams <- as_persistence_diagram_sample(list(
    as_persistence_diagram(tibble::tibble(
      birth = c(0, 1, 3, 6),
      death = c(4, 2, 8, 8)
    ))
  ))
  ds <- DiagramScaler$new(
    use = TRUE,
    scalers = list(list(c(0, 1), MinMaxScaler$new()))
  )
  dgms <- diagrams |>
    ds$fit_transform() |>
    to_ragged_tensor()

  phi <- GaussianPerslayPhi$new(
    image_size = c(5, 5),
    image_bnds = rbind(c(-.5, 1.5), c(-.5, 1.5)),
    variance = .1
  )
  weight <- GaussianMixturePerslayWeight$new(
    gaussians = matrix(c(0, 0, 1, 1), nrow = 4, ncol = 1),
  )
  perm_op <- tf$math$reduce_sum
  rho <- tf$identity

  perslay <- Perslay$new(
    phi = phi,
    weight = weight,
    perm_op = perm_op,
    rho = rho
  )

  vectors <- perslay$call(dgms)
  expect_snapshot(vectors)
})

test_that("perslay works with grid weight and Gaussian phi", {
  diagrams <- as_persistence_diagram_sample(list(
    as_persistence_diagram(tibble::tibble(
      birth = c(0, 1, 3, 6),
      death = c(4, 2, 8, 8)
    ))
  ))
  ds <- DiagramScaler$new(
    use = TRUE,
    scalers = list(list(c(0, 1), MinMaxScaler$new()))
  )
  dgms <- diagrams |>
    ds$fit_transform() |>
    to_ragged_tensor()

  phi <- GaussianPerslayPhi$new(
    image_size = c(5, 5),
    image_bnds = rbind(c(-.5, 1.5), c(-.5, 1.5)),
    variance = .1
  )
  weight <- GridPerslayWeight$new(
    grid = matrix(c(1:13, 12:1), 5, 5),
    grid_bnds = rbind(c(-.5, 1.5), c(-.5, 1.5))
  )
  perm_op <- tf$math$reduce_sum
  rho <- tf$identity

  perslay <- Perslay$new(
    phi = phi,
    weight = weight,
    perm_op = perm_op,
    rho = rho
  )

  vectors <- perslay$call(dgms)
  expect_snapshot(vectors)
})

test_that("perslay works with power weight and Gaussian phi", {
  diagrams <- as_persistence_diagram_sample(list(
    as_persistence_diagram(tibble::tibble(
      birth = c(0, 1, 3, 6),
      death = c(4, 2, 8, 8)
    ))
  ))
  ds <- DiagramScaler$new(
    use = TRUE,
    scalers = list(list(c(0, 1), MinMaxScaler$new()))
  )
  dgms <- diagrams |>
    ds$fit_transform() |>
    to_ragged_tensor()

  phi <- GaussianPerslayPhi$new(
    image_size = c(5, 5),
    image_bnds = rbind(c(-.5, 1.5), c(-.5, 1.5)),
    variance = .1
  )
  weight <- PowerPerslayWeight$new(
    constant = 1,
    power = 0
  )
  perm_op <- tf$math$reduce_sum
  rho <- tf$identity

  perslay <- Perslay$new(
    phi = phi,
    weight = weight,
    perm_op = perm_op,
    rho = rho
  )

  vectors <- perslay$call(dgms)
  expect_snapshot(vectors)
})

test_that("perslay works with Gaussian mixture weight and tent phi", {
  diagrams <- as_persistence_diagram_sample(list(
    as_persistence_diagram(tibble::tibble(
      birth = c(0, 1, 3, 6),
      death = c(4, 2, 8, 8)
    ))
  ))
  ds <- DiagramScaler$new(
    use = TRUE,
    scalers = list(list(c(0, 1), MinMaxScaler$new()))
  )
  dgms <- diagrams |>
    ds$fit_transform() |>
    to_ragged_tensor()

  phi <- TentPerslayPhi$new(
    samples = c(1:13, 12:1)
  )
  weight <- GaussianMixturePerslayWeight$new(
    gaussians = matrix(c(0, 0, 1, 1), nrow = 4, ncol = 1),
  )
  perm_op <- tf$math$reduce_sum
  rho <- tf$identity

  perslay <- Perslay$new(
    phi = phi,
    weight = weight,
    perm_op = perm_op,
    rho = rho
  )

  vectors <- perslay$call(dgms)
  expect_snapshot(vectors)
})

test_that("perslay works with grid weight and tent phi", {
  diagrams <- as_persistence_diagram_sample(list(
    as_persistence_diagram(tibble::tibble(
      birth = c(0, 1, 3, 6),
      death = c(4, 2, 8, 8)
    ))
  ))
  ds <- DiagramScaler$new(
    use = TRUE,
    scalers = list(list(c(0, 1), MinMaxScaler$new()))
  )
  dgms <- diagrams |>
    ds$fit_transform() |>
    to_ragged_tensor()

  phi <- TentPerslayPhi$new(
    samples = c(1:13, 12:1)
  )
  weight <- GridPerslayWeight$new(
    grid = matrix(c(1:13, 12:1), 5, 5),
    grid_bnds = rbind(c(-.5, 1.5), c(-.5, 1.5))
  )
  perm_op <- tf$math$reduce_sum
  rho <- tf$identity

  perslay <- Perslay$new(
    phi = phi,
    weight = weight,
    perm_op = perm_op,
    rho = rho
  )

  vectors <- perslay$call(dgms)
  expect_snapshot(vectors)
})

test_that("perslay works with power weight and tent phi", {
  diagrams <- as_persistence_diagram_sample(list(
    as_persistence_diagram(tibble::tibble(
      birth = c(0, 1, 3, 6),
      death = c(4, 2, 8, 8)
    ))
  ))
  ds <- DiagramScaler$new(
    use = TRUE,
    scalers = list(list(c(0, 1), MinMaxScaler$new()))
  )
  dgms <- diagrams |>
    ds$fit_transform() |>
    to_ragged_tensor()

  phi <- TentPerslayPhi$new(
    samples = c(1:13, 12:1)
  )
  weight <- PowerPerslayWeight$new(
    constant = 1,
    power = 0
  )
  perm_op <- tf$math$reduce_sum
  rho <- tf$identity

  perslay <- Perslay$new(
    phi = phi,
    weight = weight,
    perm_op = perm_op,
    rho = rho
  )

  vectors <- perslay$call(dgms)
  expect_snapshot(vectors)
})
