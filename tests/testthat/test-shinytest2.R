library(shinytest2)

test_that("{shinytest2} recording: joe_basic", {
  app <- AppDriver$new(variant = platform_variant(), name = "joe_basic", height = 739, 
      width = 1235)
  app$set_inputs(tabs = "tab_import")
  app$upload_file(`import-up_sr_wb_dat` = "./joe_test/stressor_response_joe.xlsx")
  app$upload_file(`import-up_sm_wb_dat` = "./joe_test/stressor_magnitude_joe.xlsx")
  app$upload_file(`import-up_sheds` = "./joe_test/joe_test_geo.gpkg")
  app$set_inputs(tabs = "tab_main_map")
  app$expect_values()
})




test_that("{shinytest2} recording: joe_involved", {
  app <- AppDriver$new(variant = platform_variant(), name = "joe_involved", height = 739, 
      width = 1235)
  app$set_inputs(tabs = "tab_main_map")
  app$set_inputs(`main_map-mainmap_shape_mouseover` = c("1701060203|ATHABASCA  ABOVE FREEMAN RIVER", 
      "0.851170518532717", "54.3126780291554", "-114.859471150286"), allow_no_input_binding_ = TRUE)
  app$set_inputs(`main_map-mainmap_shape_mouseout` = c("1701060203|ATHABASCA  ABOVE FREEMAN RIVER", 
      "0.0349767402763979", "54.2485381007421", "-114.299002094468"), allow_no_input_binding_ = TRUE)
  app$set_inputs(`main_map-Barrier_dams-shinyjs-main_map-Barrier_dams-var_id-363694015-input-click` = c(0, 
      0, 0, 1009, 264, 0, 1009, 264, 1009, 341, 0, 1, 16, 14, 0.143465275716555), 
      allow_no_input_binding_ = TRUE)
  app$set_inputs(`main_map-NN_RNTR-shinyjs-main_map-NN_RNTR-var_id-942785276-input-click` = c(0, 
      0, 0, 1029, 537, 0, 1029, 537, 1029, 614, 0, 1, 36, 14, 0.245965727942502), 
      allow_no_input_binding_ = TRUE)
  app$expect_screenshot()
  app$set_inputs(tabs = "tab_import")
  app$upload_file(`import-up_sr_wb_dat` = "./joe_test/stressor_response_joe.xlsx")
  app$upload_file(`import-up_sm_wb_dat` = "./joe_test/stressor_magnitude_joe_missing_one.xlsx")
  app$upload_file(`import-up_sheds` = "./joe_test/joe_test_geo.gpkg")
  app$set_inputs(tabs = "tab_main_map")
  app$set_inputs(`main_map-D-shinyjs-main_map-D-var_id-963061810-input-click` = c(0, 
      0, 0, 997, 327, 0, 997, 327, 997, 404, 0, 1, 9, 4, 0.817399357934217), allow_no_input_binding_ = TRUE)
  app$set_inputs(`main_map-C-shinyjs-main_map-C-var_id-118539500-input-click` = c(0, 
      0, 0, 1002, 294, 0, 1002, 294, 1002, 371, 0, 1, 9, 4, 0.442172601129239), allow_no_input_binding_ = TRUE)
  app$set_inputs(`main_map-B-shinyjs-main_map-B-var_id-250046972-input-click` = c(0, 
      0, 0, 1000, 260, 0, 1000, 260, 1000, 337, 0, 1, 7, 10, 0.627364635892296), 
      allow_no_input_binding_ = TRUE)
  app$set_inputs(`main_map-C-shinyjs-main_map-C-var_id-118539500-input-click` = c(0, 
      0, 0, 1002, 294, 0, 1002, 294, 1002, 371, 0, 1, 9, 4, 0.327209771808475), allow_no_input_binding_ = TRUE)
  app$expect_values()
})


test_that("{shinytest2} recording: run_joe", {
  app <- AppDriver$new(name = "run_joe", height = 739, width = 1235)
  app$set_inputs(tabs = "tab_main_map")
  app$click("main_map-huc_results-run_joe_model-open_joe_modal_form")
  app$click("main_map-huc_results-run_joe_model-selectall")
  app$click("main_map-huc_results-run_joe_model-go_button_run_joe")
  app$set_inputs(`main_map-huc_results-run_joe_model-check_box_group` = character(0))
  app$set_inputs(`main_map-huc_results-run_joe_model-number_of_simulations` = 100)
  app$set_inputs(`main_map-huc_results-run_joe_model-name_of_simulation` = "Default")
  app$click("main_map-huc_results-run_joe_model-go_button_run_joe")
  app$set_inputs(`main_map-mainmap_bounds` = c(55.3416418301333, -113.291015625, 
      51.7542400740335, -120.552978515625), allow_no_input_binding_ = TRUE)
  app$set_inputs(`main_map-mainmap_center` = c(-116.921997070313, 53.5859836545598), 
      allow_no_input_binding_ = TRUE)
  app$set_inputs(`main_map-Nat_lim_other-shinyjs-main_map-Nat_lim_other-var_id-994037069-input-click` = c(0, 
      0, 0, 1001, 294, 0, 1001, 544, 1001, 371, 0, 1, 21, 18, 0.256014341247733), 
      allow_no_input_binding_ = TRUE)
  app$set_inputs(`main_map-NN_RNTR-shinyjs-main_map-NN_RNTR-var_id-383971861-input-click` = c(0, 
      0, 0, 1000, 319, 0, 1000, 570, 1000, 396, 0, 1, 20, 5, 0.623602068137911), 
      allow_no_input_binding_ = TRUE)
  app$click("main_map-Nat_lim_other-response_plot")
  app$set_inputs(`main_map-Nat_lim_other-shinyjs-main_map-Nat_lim_other-var_id-994037069-input-click` = c(0, 
      0, 0, 1087, 277, 0, 1087, 527, 1087, 354, 0, 1, 5, 0, 0.111338944491989), allow_no_input_binding_ = TRUE)
  app$click("main_map-Nat_lim_other-close_sr_modal")
  app$set_inputs(`main_map-Nat_lim_other-stressor_response_dt_rows_current` = c(1, 
      2, 3, 4, 5, 6), allow_no_input_binding_ = TRUE)
  app$set_inputs(`main_map-Nat_lim_other-stressor_response_dt_rows_all` = c(1, 2, 
      3, 4, 5, 6), allow_no_input_binding_ = TRUE)
  app$set_inputs(`main_map-Nat_lim_other-stressor_response_dt_state` = c(1699482561728, 
      0, 500, "", TRUE, FALSE, TRUE, c(TRUE, "", TRUE, FALSE, TRUE), c(TRUE, "", 
          TRUE, FALSE, TRUE), c(TRUE, "", TRUE, FALSE, TRUE), c(TRUE, "", TRUE, FALSE, 
          TRUE), c(TRUE, "", TRUE, FALSE, TRUE)), allow_no_input_binding_ = TRUE)
  app$set_inputs(`main_map-Nat_lim_other-stressor_response_dt_rows_selected` = 5, 
      allow_no_input_binding_ = TRUE)
  app$set_inputs(`main_map-Nat_lim_other-stressor_response_dt_row_last_clicked` = 5, 
      allow_no_input_binding_ = TRUE, priority_ = "event")
  app$set_inputs(`main_map-Nat_lim_other-stressor_response_dt_cell_clicked` = c(5, 
      1, 80), allow_no_input_binding_ = TRUE, priority_ = "event")
  app$set_inputs(`main_map-Nat_lim_other-stressor_response_dt_rows_selected` = character(0), 
      allow_no_input_binding_ = TRUE)
  app$set_inputs(`main_map-Nat_lim_other-stressor_response_dt_row_last_clicked` = 5, 
      allow_no_input_binding_ = TRUE, priority_ = "event")
  app$set_inputs(`main_map-Nat_lim_other-stressor_response_dt_cell_clicked` = c(5, 
      1, 80), allow_no_input_binding_ = TRUE, priority_ = "event")
  app$set_inputs(`main_map-Nat_lim_other-stressor_response_dt_rows_selected` = 6, 
      allow_no_input_binding_ = TRUE)
  app$set_inputs(`main_map-Nat_lim_other-stressor_response_dt_row_last_clicked` = 6, 
      allow_no_input_binding_ = TRUE, priority_ = "event")
  app$set_inputs(`main_map-Nat_lim_other-stressor_response_dt_cell_clicked` = c(6, 
      1, 100), allow_no_input_binding_ = TRUE, priority_ = "event")
  app$set_inputs(`main_map-Nat_lim_other-stressor_response_dt_rows_selected` = character(0), 
      allow_no_input_binding_ = TRUE)
  app$set_inputs(`main_map-Nat_lim_other-stressor_response_dt_row_last_clicked` = 6, 
      allow_no_input_binding_ = TRUE, priority_ = "event")
  app$set_inputs(`main_map-Nat_lim_other-stressor_response_dt_cell_clicked` = c(6, 
      1, 100), allow_no_input_binding_ = TRUE, priority_ = "event")
  app$set_inputs(`main_map-Nat_lim_other-stressor_response_dt_rows_selected` = 6, 
      allow_no_input_binding_ = TRUE)
  app$set_inputs(`main_map-Nat_lim_other-stressor_response_dt_row_last_clicked` = 6, 
      allow_no_input_binding_ = TRUE, priority_ = "event")
  app$set_inputs(`main_map-Nat_lim_other-stressor_response_dt_cell_clicked` = c(6, 
      1, 100), allow_no_input_binding_ = TRUE, priority_ = "event")
  app$set_inputs(`main_map-Nat_lim_other-stressor_response_dt_rows_selected` = 5, 
      allow_no_input_binding_ = TRUE)
  app$set_inputs(`main_map-Nat_lim_other-stressor_response_dt_row_last_clicked` = 5, 
      allow_no_input_binding_ = TRUE, priority_ = "event")
  app$set_inputs(`main_map-Nat_lim_other-stressor_response_dt_cell_edit` = c(6, 1, 
      90), allow_no_input_binding_ = TRUE, priority_ = "event")
  app$set_inputs(`main_map-Nat_lim_other-stressor_response_dt_cell_clicked` = c(5, 
      1, 80), allow_no_input_binding_ = TRUE, priority_ = "event")
  app$set_inputs(`main_map-Nat_lim_other-stressor_response_dt_rows_current` = c(1, 
      2, 3, 4, 5, 6), allow_no_input_binding_ = TRUE)
  app$set_inputs(`main_map-Nat_lim_other-stressor_response_dt_rows_all` = c(1, 2, 
      3, 4, 5, 6), allow_no_input_binding_ = TRUE)
  app$set_inputs(`main_map-Nat_lim_other-stressor_response_dt_state` = c(1699482565884, 
      0, 500, "", TRUE, FALSE, TRUE, c(TRUE, "", TRUE, FALSE, TRUE), c(TRUE, "", 
          TRUE, FALSE, TRUE), c(TRUE, "", TRUE, FALSE, TRUE), c(TRUE, "", TRUE, FALSE, 
          TRUE), c(TRUE, "", TRUE, FALSE, TRUE)), allow_no_input_binding_ = TRUE)
  app$expect_values()
})
