test_that("package documentation is accessible and complete", {
  # Verify that easyModels documentation topics exist
  help_pkg <- help("easyModels", package = "easyModels")
  expect_true(length(help_pkg) > 0)
  
  help_pkg_alias <- help("easyModels-package", package = "easyModels")
  expect_true(length(help_pkg_alias) > 0)
})
