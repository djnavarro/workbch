#' Initialise by searching for git repos
#'
#' @param proj_dir where to look for git repositories
#' @param workbch_dir where to store workbench files
#'
#' @export
initialise_by_git <- function(proj_dir, workbch_dir = getOption("workbch.home")) {

  # ensure that we have set the workbench home directory
  set_home(workbch_dir)

  # find all git repositories
  repos <- list.files(
    path = proj_dir, pattern = "\\.git$", full.names = TRUE,
    recursive = TRUE, include.dirs = TRUE, all.files = TRUE)
  paths <- normalizePath(gsub("\\.git$", "", repos))

  # guess the job name from repo name
  job_names <- gsub("^.*[/\\]", "", paths)

  # create minimal person listing
  set_person("me", "me")

  # now create
  purrr::walk2(
    .x = paths,
    .y = job_names,
    .f = function(.x, .y) {
      cat("adding: ", .x, "\n")
      set_job(
        name = .y,
        description = .y,
        owner = "me",
        path = .x
        )
    })

}
