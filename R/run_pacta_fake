# Fake PACTA Runner, useful for testing and debugging
#' @export
run_pacta_fake <- function(
  working_dir,
  user_dir,
  duration = 10,
  dirs_to_populate = c(
    "30_Processed_Inputs",
    "40_Results",
    "50_Outputs"
  )
  ) {
  log_warning("This is a fake PACTA Runner. It does not generate real results")
  log_info("Beginning PACTA process (sleeping for {duration} seconds)")
  Sys.sleep(duration)
  file.create(file.path(working_dir, dirs_to_populate, "FAKE-RESULTS.txt"))
  log_info("PACTA ran sucessfully (slept for {duration} seconds)")
  #Simulate a sucessful exit
  return(0)
}
