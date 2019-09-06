
#' Report the git status of all jobs
#'
#' @param show_clean should clean repos be included?
#'
#' @return A tibble
#' @export
job_gitreport <- function(show_clean = FALSE) {

  # get the job locations
  proj <- job_allpaths(show_hidden = TRUE)
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
