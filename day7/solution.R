input <- readr::read_lines("day7/input")
input_cln <- input[!stringr::str_detect(input, "\\$ ls")]

change_dir <- function(old_dir, new_dir) {
  if (new_dir == "..") {
    parent_folder <- dirname(old_dir)

    if (nchar(parent_folder) == 1) {
      folder_out <- "/"
    } else {
      folder_out <- parent_folder
    }
  } else if (new_dir == "/") {
    folder_out <- "/"
  } else {
    new_path <- file.path(
      old_dir, new_dir
    )

    folder_out <- stringr::str_replace(
      new_path, "//", "/"
    )
  }

  folder_out
}

# Part 1
current_dir <- ""
dir_info <- list()

purrr::walk(
  input_cln,
  ~ {
    if (stringr::str_detect(.x, "\\$ cd")) {
      # Change dir command
      current_dir <<- change_dir(
        current_dir,
        stringr::str_split(.x, "\\$ cd ")[[1]][2]
      )
    } else if (stringr::str_detect(.x, "dir")) {
      # Directory
      child_dir <- file.path(
        current_dir,
        stringr::str_split(.x, "dir ")[[1]][2]
      )

      child_dir <- stringr::str_replace(
        child_dir, "//", "/"
      )

      if (!child_dir %in% names(dir_info)) {
        dir_info[[child_dir]] <<- 0
      }
    } else {
      # File
      file_info <- stringr::str_split(.x, " ")[[1]]

      if (current_dir %in% names(dir_info)) {
        dir_info[[current_dir]] <<- sum(
          dir_info[[current_dir]],
          as.numeric(file_info[[1]])
        )
      } else {
        dir_info[[current_dir]] <<- as.numeric(file_info[[1]])
      }
    }
  }
)

dir_totals <- purrr::map_dbl(
  sort(names(dir_info)),
  ~ {
    child_dirs <- stringr::str_which(
      names(dir_info),
      paste0("^", .x, "/")
    )

    if (length(child_dirs)) {
      sum(c(unlist(dir_info[child_dirs]), dir_info[[.x]]))
    } else {
      dir_info[[.x]]
    }
  }
)

message("Total: ", sum(dir_totals[dir_totals < 100000]))

# Part 2
free_space <- 70000000 - sum(unlist(dir_info))
space_needed <- 30000000 - free_space

message("Drive size: ", min(dir_totals[dir_totals >= space_needed]))
