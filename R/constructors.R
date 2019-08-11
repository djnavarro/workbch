# constructor functions for jobs, tasks, and urls.


# constructors for new objects with default values ------------------------

new_job <- function(jobname, description, owner, status = NULL,
                    team = NULL, priority = NULL, deadline = NULL,
                    tags = NULL, path = NULL, urls = NULL,
                    tasks = NULL, verify = TRUE) {

  # replace nulls with default values
  status <- status %||% "active"
  team <- team %||% character(0)
  priority <- priority %||% 1
  deadline <- deadline %||% NA_character_
  tags <- tags %||% character(0)
  path <- path %||% NA_character_
  urls <- urls %||% empty_url()
  tasks <- tasks %||% empty_task()

  # verify unless explicitly told not to
  if(verify) {
    verify_jobname(jobname)
    verify_description(description)
    verify_owner(owner)
    verify_status(status)
    ## missing: verify_team(team)
    ## missing: verify_tags(tags)
    verify_path(path)
    verify_priority(priority)
    verify_deadline(deadline)
    ## missing: verify_urls(urls)
    ## missing: verify_tasks(tasks)
  }

  # check and tidy names
  owner <- ppl_parseowner(owner)
  team <- ppl_fullname(team)
  if(!(owner %in% team)) {
    team <- c(owner, team)
  }

  # construct object
  list(
    jobname = jobname,
    description = description,
    owner = owner,
    status = status,
    team = team,
    priority = priority,
    deadline = deadline,
    tags = tags,
    path = path,
    urls = urls,
    tasks = tasks
  )
}



new_task <- function(jobname, id, description, owner, status = NULL,
                     priority = NULL, deadline = NULL, verify = TRUE) {

  # replace nulls with default values
  status <- status %||% "active"
  priority <- priority %||% 1
  deadline <- deadline %||% NA_character_

  # verify unless explicitly told not to
  if(verify) {
    verify_jobname(jobname)
    ### missing: verify_id(id)
    verify_description(description)
    verify_owner(owner)
    verify_status(status)
    verify_priority(priority)
    verify_deadline(deadline)
  }

  # construct object
  tibble::tibble(
    jobname = jobname,
    id = id,
    description = description,
    owner = owner,
    status = status,
    priority = priority,
    deadline = deadline
  )
}

new_url <- function(site, link, verify = TRUE) {

  # verify unless explicitly told not to
  if(verify) {
    verify_site(site)
    verify_link(link)
  }

  # construct object
  tibble::tibble(
    site = site,
    link = link
  )
}



# constructors for empty objects ------------------------------------------

# these sometimes skip the verification step because in normal usage the
# user is expected to specify a string of length one (enforced in the
# verification functions) but strictly speaking the empty object violates
# this by using strings of length 0

empty_task <- function() {
  new_task(jobname = character(0), id = numeric(0), description = character(0),
           owner = character(0), status = character(0), priority = numeric(0),
           deadline = character(0), verify = FALSE)
}

empty_url <- function() {
  new_url(site = character(0), link = character(0), verify = FALSE)
}


empty_job <- function() {
  new_job(jobname = character(0), description = character(0),
          owner = character(0), status = character(0), team = character(0),
          priority = numeric(0), deadline = character(0), tags = character(0),
          path = character(0), urls = empty_url(), tasks = empty_task())
}
