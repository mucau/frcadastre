test_that("idu_get_feuille() works offline with mocked dependencies", {
  fake_idus <- c("721870000A0001", "721870000A0002")

  fake_parts <- data.frame(
    idu   = fake_idus,
    insee = c("72187", "72187"),
    stringsAsFactors = FALSE
  )

  fake_feuilles <- sf::st_sf(
    id   = c("721870000A", "721870000B"),
    commune = c("72187", "72187"),
    geometry = sf::st_sfc(sf::st_point(c(1, 1)), sf::st_point(c(2, 2)))
  )

  with_mocked_bindings(
    idu_check  = function(idu, error = TRUE) TRUE,
    idu_split  = function(idu) fake_parts,
    get_etalab = function(insee_codes, layer = NULL, ...) fake_feuilles,
    {
      # Test vector output
      res_vec <- idu_get_feuille(fake_idus, result_as_list = FALSE)
      expect_type(res_vec, "character")
      expect_true(all(res_vec %in% fake_feuilles$id))

      # Test list output
      res_list <- idu_get_feuille(fake_idus, result_as_list = TRUE)
      expect_type(res_list, "list")
      expect_named(res_list, unique(fake_parts$insee))
      expect_true(all(unlist(res_list) %in% fake_feuilles$id))
    }
  )
})


test_that("idu_get_feuille() works online with httptest2 mocks", {
  skip_if_not_installed("httptest2")

  httptest2::with_mock_dir("idu_get_feuille", {
    res_flat <- idu_get_feuille("721870000A0001")
    expect_type(res_flat, "character")
    expect_true(length(res_flat) >= 1)

    res_list <- idu_get_feuille(c("721870000A0001", "721870000A0002", "721880000A0001"), result_as_list = TRUE)
    expect_type(res_list, "list")
    expect_named(res_list)
  })
})
