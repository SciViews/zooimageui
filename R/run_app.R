#' Run the Shiny Application
#'
#' @param ... arguments to pass to golem_opts. 
#' See `?golem::get_golem_options` for more details.
#' @inheritParams shiny::shinyApp
#'
#' @export
#' @importFrom shiny shinyApp
#' @importFrom golem with_golem_options 
run_app <- function(
  onStart = NULL,
  options = list(), 
  enableBookmarking = NULL,
  uiPattern = "/",
  data_folder_path = Sys.getenv("ZOOIMAGE_DATA_DIR"),
  smpfiles = list.files(paste0(data_folder_path,"/Samples")),
  smps = samples(smpfiles),
  ...
) {
  with_golem_options(
    app = shinyApp(
      ui = app_ui,
      server = app_server,
      onStart = onStart,
      options = options, 
      enableBookmarking = enableBookmarking, 
      uiPattern = uiPattern
    ), 
    golem_opts = list(
      data_folder_path = data_folder_path,
      smpfiles = smpfiles,
      smps = smps
    )
  )
}
