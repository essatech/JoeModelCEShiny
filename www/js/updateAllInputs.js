/* note that js function should not include any line comments (they break the code)*/
shinyjs.updateAllInputs = function (data) {

  let rVs = JSON.parse(data);

  console.log("Updating life cycle parameters UI inputs...");
  console.log(rVs);

  /* Update all inputs in module_matrix_model_inputs_ui */
  /* continue for Nstage, k, events, eps, int, SE, S0, SR, surv_1, surv_2, surv_3, surv_4, year_1, year_2, year_3, year_4, cr_E, cr_0, cr_1, cr_2, cr_3, cr_4, mat_1, mat_2, mat_3, mat_4, eps_sd, egg_rho, M.cv, M.rho */
  
  document.getElementById("matrix_model-mm_inputs-k").value = rVs.Value[rVs.Name.indexOf('k')];
  document.getElementById("matrix_model-mm_inputs-events").value = rVs.Value[rVs.Name.indexOf('events')];
  document.getElementById("matrix_model-mm_inputs-eps").value = rVs.Value[rVs.Name.indexOf('eps')];
  document.getElementById("matrix_model-mm_inputs-int").value = rVs.Value[rVs.Name.indexOf('int')];
  document.getElementById("matrix_model-mm_inputs-SE").value = rVs.Value[rVs.Name.indexOf('SE')];
  document.getElementById("matrix_model-mm_inputs-S0").value = rVs.Value[rVs.Name.indexOf('S0')];
  document.getElementById("matrix_model-mm_inputs-SR").value = rVs.Value[rVs.Name.indexOf('SR')];
  document.getElementById("matrix_model-mm_inputs-surv_1").value = rVs.Value[rVs.Name.indexOf('surv_1')];
  document.getElementById("matrix_model-mm_inputs-surv_2").value = rVs.Value[rVs.Name.indexOf('surv_2')];
  document.getElementById("matrix_model-mm_inputs-surv_3").value = rVs.Value[rVs.Name.indexOf('surv_3')];
  document.getElementById("matrix_model-mm_inputs-surv_4").value = rVs.Value[rVs.Name.indexOf('surv_4')];
  document.getElementById("matrix_model-mm_inputs-year_1").value = rVs.Value[rVs.Name.indexOf('year_1')];
  document.getElementById("matrix_model-mm_inputs-year_2").value = rVs.Value[rVs.Name.indexOf('year_2')];
  document.getElementById("matrix_model-mm_inputs-year_3").value = rVs.Value[rVs.Name.indexOf('year_3')];
  document.getElementById("matrix_model-mm_inputs-year_4").value = rVs.Value[rVs.Name.indexOf('year_4')];
  document.getElementById("matrix_model-mm_inputs-cr_E").value = rVs.Value[rVs.Name.indexOf('cr_E')];
  document.getElementById("matrix_model-mm_inputs-cr_0").value = rVs.Value[rVs.Name.indexOf('cr_0')];
  document.getElementById("matrix_model-mm_inputs-cr_1").value = rVs.Value[rVs.Name.indexOf('cr_1')];
  document.getElementById("matrix_model-mm_inputs-cr_2").value = rVs.Value[rVs.Name.indexOf('cr_2')];
  document.getElementById("matrix_model-mm_inputs-cr_3").value = rVs.Value[rVs.Name.indexOf('cr_3')];
  document.getElementById("matrix_model-mm_inputs-cr_4").value = rVs.Value[rVs.Name.indexOf('cr_4')];
  document.getElementById("matrix_model-mm_inputs-mat_1").value = rVs.Value[rVs.Name.indexOf('mat_1')];
  document.getElementById("matrix_model-mm_inputs-mat_2").value = rVs.Value[rVs.Name.indexOf('mat_2')];
  document.getElementById("matrix_model-mm_inputs-mat_3").value = rVs.Value[rVs.Name.indexOf('mat_3')];
  document.getElementById("matrix_model-mm_inputs-mat_4").value = rVs.Value[rVs.Name.indexOf('mat_4')];
  document.getElementById("matrix_model-mm_inputs-eps_sd").value = rVs.Value[rVs.Name.indexOf('eps_sd')];
  document.getElementById("matrix_model-mm_inputs-egg_rho").value = rVs.Value[rVs.Name.indexOf('egg_rho')];
  document.getElementById("matrix_model-mm_inputs-M.cv").value = rVs.Value[rVs.Name.indexOf('M.cv')];
  document.getElementById("matrix_model-mm_inputs-M.rho").value = rVs.Value[rVs.Name.indexOf('M.rho')];

  console.log("Values updated with JS!");
};