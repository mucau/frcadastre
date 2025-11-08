test_that("idu_check returns logical TRUE vector when all IDUs are valid", {
  idus <- c("721870000A0001", "971020000B0002")
  result <- idu_check(idus)
  expect_true(all(result))
})

test_that("idu_check throws an error for a single invalid IDU (default error = TRUE)", {
  expect_error(
    idu_check("invalidIDU"),
    "Invalid IDU"
  )
})

test_that("idu_check throws an error listing multiple invalid IDUs", {
  idus <- c("721870000A0001", "invalid1", "invalid2")
  expect_error(
    idu_check(idus),
    "Invalid IDU\\(s\\) detected: invalid1, invalid2"
  )
})

test_that("idu_check detects NA and empty strings as invalid", {
  expect_error(
    idu_check(c("721870000A0001", NA, "")),
    "Invalid IDU"
  )
})

test_that("idu_check correctly identifies valid formats", {
  valid_idus <- c(
    "721870000A0001", # typical
    "971020000B0002", # department 97X
    "012340000Z9999"  # section letter Z
  )
  result <- idu_check(valid_idus)
  expect_true(all(result))
})

test_that("idu_check flags invalid formats correctly", {
  invalid_idus <- c(
    "72187",           # too short
    "72X870000A0001",  # invalid char in dep
    "721870000a0001",  # lowercase
    "721870000!0001",  # special char
    "721870000AA001",  # too short (13)
    "721870000AA00011" # too long (15)
  )
  expect_error(idu_check(invalid_idus), "Invalid IDU")
})

test_that("idu_check handles single valid IDU without error", {
  expect_true(all(idu_check("721870000A0001")))
})

test_that("idu_check throws error when NA or empty string", {
  expect_error(idu_check(NA), "Invalid IDU")
  expect_error(idu_check(""), "Invalid IDU")
})

test_that("idu_check returns logical vector with warning when error = FALSE", {
  idus <- c("721870000A0001", "invalid")
  expect_warning(
    result <- idu_check(idus, error = FALSE),
    "Invalid IDU"
  )
  expect_equal(result, c(TRUE, FALSE))
})

test_that("idu_check with error = FALSE returns all TRUE for valid input", {
  idus <- c("721870000A0001", "971020000B0002")
  expect_silent(result <- idu_check(idus, error = FALSE))
  expect_true(all(result))
})
