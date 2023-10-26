library(shinytest2)

test_that("Initial snapshot values are consistent", {
  app <- AppDriver$new(name = "init")
  app$expect_values()
})


test_that("Module values are consistent", {
  app <- AppDriver$new(name = "mod")
  app$click("examplemodule1-button")
  app$click("examplemodule1-button")
  app$expect_values()
})


test_that("{shinytest2} recording: JoeModelCEShiny", {
  app <- AppDriver$new(variant = platform_variant(), name = "JoeModelCEShiny", height = 746, 
      width = 1235)
  app$set_inputs(tabs = "tab_import")
  app$expect_values()
  app$expect_values()
  rlang::warn(paste0("``import-up_sr_wb_dat`` should be the path to the file, relative to the app's tests/testthat directory.\n", 
      "Remove this warning when the file is in the correct location."))
  app$upload_file(`import-up_sr_wb_dat` = "stressor_response_matrix_AB.xlsx")
  rlang::warn(paste0("``import-up_sm_wb_dat`` should be the path to the file, relative to the app's tests/testthat directory.\n", 
      "Remove this warning when the file is in the correct location."))
  app$upload_file(`import-up_sm_wb_dat` = "stressor_magnitude_matrix.xlsx")
  rlang::warn(paste0("``import-up_sheds`` should be the path to the file, relative to the app's tests/testthat directory.\n", 
      "Remove this warning when the file is in the correct location."))
  app$upload_file(`import-up_sheds` = c("poly.cpg", "poly.dbf", "poly.prj", "poly.shp", 
      "poly.shx"))
  app$set_inputs(tabs = "tab_main_map")
  app$expect_screenshot()
})
