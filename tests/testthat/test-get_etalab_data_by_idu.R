test_that("get_etalab_data_by_idu() works offline with mocked dependencies", {
  skip_if_not_installed("sf")

  # Fake IDU parts
  fake_idu_parts <- data.frame(
    idu = c("72181000AB0001", "72181000AB0002"),
    insee = c("72181", "72181"),
    stringsAsFactors = FALSE
  )

  # Mock get_etalab to return a fake sf
  fake_sf <- sf::st_sf(
    id = 1:2,
    geometry = sf::st_sfc(sf::st_point(c(0, 0)), sf::st_point(c(1, 1)))
  )

  with_mocked_bindings(
    idu_check = function(idu) TRUE,
    idu_split = function(idu) fake_idu_parts,
    get_etalab = function(insee_codes, layer, verbose = TRUE) fake_sf,
    {
      res <- get_etalab_data_by_idu(
        c("72181000AB0001", "72181000AB0002"),
        layer = "parcelles",
        verbose = TRUE
      )
      expect_s3_class(res, "sf") # now returns sf directly
      expect_true(all(fake_idu_parts$insee %in% unique(fake_idu_parts$insee)))
    }
  )
})


test_that("get_etalab_data_by_idu() works online with httptest2", {
  skip_if_not_installed("httptest2")
  skip_if_not_installed("sf")

  httptest2::with_mock_dir("get_etalab_data_by_idu", {
    # Single IDU - silent mode
    expect_silent({
      res1 <- get_etalab_data_by_idu(
        "72181000AB0001",
        layer = "parcelles",
        verbose = FALSE
      )
    })
    expect_s3_class(res1, "sf")

    # Multiple IDUs - verbose mode
    expect_message({
      res2 <- get_etalab_data_by_idu(
        c("72181000AB0001", "72181000AB0002"),
        layer = "parcelles",
        verbose = TRUE
      )
    })
    expect_s3_class(res2, "sf")
  })
})


test_that("get_etalab_data_by_idu() stops on invalid IDUs", {
  skip_if_not_installed("sf")

  expect_error(
    get_etalab_data_by_idu("7218100A", layer = "parcelles", verbose = TRUE),
    regexp = "Invalid IDU"
  )
})
