# Known helpful settings
#' @export
default_docker_args <- function() {
  log_debug("Using Default docker arguments.")
  c(
    "--rm",
    "--network none",
    "--user 1000:1000",
    "--memory-swappiness=0"
  )
}

#' @export
docker_mount <-  function(local_path, target_path) {
  if (!file.exists(local_path)) {
    log_warn(
      paste(
        "Local path",
        "{local_path}",
        "does not exist prior to mounting as docker volume."
      )
    )
  }
  paste0(
    "--mount type=bind,source=",
    local_path,
    ",target=",
    target_path
  )
}

#' @export
run_pacta_docker <- function(
  working_dir,
  user_dir,
  script_to_run = "/bound/bin/run-r-scripts",
  script_arguments = "",
  args = c(
    default_docker_args,
    docker_mount(
      local_path = working_dir,
      target_path = "/bound/working_dir"
      ),
    docker_mount(
      local_path = user_dir,
      target_path = "/user_results"
    ),
  docker_image = "transitionmonitordockerregistry.azurecr.io/rmi_pacta",
  docker_tag = "latest",
  copy = TRUE
  )
  ) {

  exit_code <- system2(
    command = "docker",
    args = c(
      "run",
      docker_args,
      paste0(docker_image, ":", docker_tag),
      script_to_run,
      script_arguments
    )
  )

  if (exit_code == 0) {
    log_info("Docker process exited with status {exit_code}")
  } else {
    log_error("Docker process exited with status {exit_code}")
  }

  return(exit_code)
}
