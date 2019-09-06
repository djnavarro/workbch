# defines the "view" family of functions

#' View a list of jobs
#'
#' @param priority what priority levels to show in the output
#' @param status what status values to show in the output
#' @param owner what owner values to show in the output
#' @param tags what tags to show in the output
#' @param cols what columns to show in the output
#' @export
view_jobs <- function(
  priority = 1:2,
  status = c("active", "inactive"),
  owner = NULL,
  tags = NULL,
  cols = c("jobname", "owner", "priority", "status", "description")
){

  # read jobs
  jobs <- job_read()

  # construct tibble with the simple fields
  job_tbl <- purrr::map_df(jobs, function(x){
    tibble::as_tibble(x[c("jobname", "owner", "priority", "status",
                          "description", "path")])})

  # add tags
  job_tbl$tags <- purrr::map_chr(jobs, function(x) {
    paste0(x$tags, collapse = ";")
  })

  # add urls
  job_tbl$urls <- purrr::map(jobs, function(x) {
    x$urls
  })

  # arrange
  job_tbl <- dplyr::arrange(job_tbl, priority, status, owner, jobname)

  # drop rows as needed
  if(!is.null(status)) job_tbl <- job_tbl[job_tbl$status %in% status, ]
  if(!is.null(priority)) job_tbl <- job_tbl[job_tbl$priority %in% priority, ]
  if(!is.null(owner)) job_tbl <- job_tbl[job_tbl$owner %in% owner, ]

  if(nrow(job_tbl) == 0) return(NULL)

  # drop cols as needed
  if(!is.null(cols)) job_tbl <- job_tbl[, cols]

  # check job paths to warn user
  job_checksentinels()

  return(as_wkbch_tbl(job_tbl))
}


#' View the git status of jobs
#'
#' @param show_hidden should hidden jobs be included?
#' @param show_clean should clean repos be included?
#'
#' @return A tibble
#' @export
view_git <- function(show_hidden = TRUE, show_clean = FALSE) {

  # get the job locations
  proj <- job_allpaths(show_hidden = show_hidden)
  x <- list()

  for(i in 1:nrow(proj)) {

    # for the sake of my sanity
    pp <- proj$path[i]
    pn <- proj$jobname[i]

    if(git2r::in_repository(pp)) {

      # get the branch
      repo_head <- git2r::repository_head(pp)
      upstream_head <- git2r::branch_get_upstream(repo_head)

      # number of commits ahead and behind
      if(is.null(upstream_head)) {
        repo_ab <- c(NA, NA)
      } else {
        repo_ab <- git2r::ahead_behind(
          local = repo_head,
          upstream = upstream_head
        )
      }

      # get the repo git status
      repo_status <- purrr::map_dfr(unclass(git2r::status(pp)), length)

      # put it all together in a tibble
      x[[i]] <- dplyr::bind_cols(
        tibble::tibble(jobname = pn),
        repo_status,
        tibble::tibble(ahead = repo_ab[1], behind = repo_ab[2])
      )
    }
  }

  # collapse to a single tibble
  gitst <- dplyr::bind_rows(x)

  if(!show_clean) {
    gitst <- dplyr::filter(gitst,
                           !(staged == 0 & unstaged == 0 & untracked == 0 &
                               (is.na(ahead) | ahead == 0) & (is.na(behind) | behind == 0))
    )
  }

  return(as_wkbch_tbl(gitst))

}

