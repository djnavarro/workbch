
#' Report the git status of all jobs
#'
#' @param show_clean should clean repositories be included in the output?
#'
#' @details The role of the \code{git_report()} function is to provide an overview
#' of the status of all workbch jobs that are associated with a git repository.
#' For every job, it uses \code{git2r::in_repository} to determine if the job
#' folder (i.e., the \code{path} for that job) is in a git repository. Jobs that are
#' not in git repositories are ignored.
#'
#' For jobs that are associated with git repositories, the \code{git_report()}
#' function calls \code{git2r::status()} to determine the git status. If there is an
#' upstream set (i.e., \code{git2r::branch_get_upstream()} detects an upstream
#' repository), it will also call \code{git2r::ahead_behind()} to determine how many
#' commits the local repository is ahead and/or behind of the upstream.
#'
#' By default, no output is shown for clean repositories (\code{show_clean = FALSE}).
#' A repository is deemed to be clean if there are no staged, unstaged or
#' untracked files and it is neither ahead nor behind the upstream repository.
#' If the user specifies \code{show_clean = TRUE}, then results are reported for every
#' job that is linked to a git repository.
#'
#' @return A tibble with columns \code{jobname}, \code{staged}, \code{unstaged},
#' \code{untracked}, \code{ahead} and \code{behind}. The \code{jobname} column is
#' a character vector, all others are integer valued.
#'
#' @export
job_gitreport <- function(show_clean = FALSE) {

  # get the job locations
  jobs <- job_read()
  proj <- pull_jobinfo(jobs)
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
