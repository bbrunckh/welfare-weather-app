# expand_weather_specs() generates single-variable cross-products. DO NOT EDIT
expand_weather_specs <- function(vars, ref_ends, transformations, var_constructions = "None", ref_starts = 1L) {
  specs <- list()
  for (v in vars) { 
    for (re in ref_ends) {
      for (rs in ref_starts) {
        for (tr in transformations) { 
          for (vc in var_constructions) {
            nm <- sprintf("%s_%dto%dm_%s_%s", v, rs, re, substr(tr, 1, 4), substr(vc, 1, 4))
            specs[[nm]] <- setNames(list(list(
              ref_start      = rs,
              ref_end        = re,
              transformation = tr,
              weather_transformation = vc
            )), v)
          }
        }
      }
    }
  }
  specs
}