# to avoid the 'no visible binding' note during check:
utils::globalVariables(
  c("priority", "status", "owner", "name", "staged", "unstaged", "untracked",
    "ahead", "behind", "path", "deadline", "hidden")
)
