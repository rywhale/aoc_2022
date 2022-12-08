input <- readr::read_lines("day8/input")
input_sp <- stringr::str_split(input, "")

input_mat <- matrix(
  as.numeric(unlist(input_sp)),
  ncol = nchar(input[[1]])
)

input_mat <- t(input_mat)

# Part 1
all_coords <- purrr::cross2(
  1:nrow(input_mat), 1:ncol(input_mat)
)

is_visible <- purrr::map_lgl(
  all_coords,
  ~ {
    curr_val <- input_mat[.x[[1]], .x[[2]]]
    curr_row <- .x[[1]]
    curr_col <- .x[[2]]

    if (.x[[1]] %in% c(1, nrow(input_mat)) | .x[[2]] %in% c(1, ncol(input_mat))) {
      TRUE
    } else {
      row_check <- c(
        all(input_mat[curr_row, ][1:curr_col - 1] < curr_val),
        all(input_mat[curr_row, ][(curr_col + 1):ncol(input_mat)] < curr_val)
      )

      col_check <- c(
        all(input_mat[, curr_col][1:curr_row - 1] < curr_val),
        all(input_mat[, curr_col][(curr_row + 1):nrow(input_mat)] < curr_val)
      )

      any(row_check) | any(col_check)
    }
  }
)

message("Total visible: ", sum(is_visible))

calc_view <- function(neighbour_vals, curr_val) {
  if (all(neighbour_vals < curr_val)) {
    length(neighbour_vals)
  } else {
    first_exceed <- which(neighbour_vals >= curr_val)[[1]]
    length(1:first_exceed)
  }
}

# Part 2
view_dist <- purrr::map_dbl(
  all_coords,
  ~ {
    curr_val <- input_mat[.x[[1]], .x[[2]]]
    curr_row <- .x[[1]]
    curr_col <- .x[[2]]

    if (.x[[1]] %in% c(1, nrow(input_mat)) | .x[[2]] %in% c(1, ncol(input_mat))) {
      0
    } else {
      view_up <- calc_view(
        rev(input_mat[, curr_col][1:(curr_row - 1)]),
        curr_val
      )

      view_down <- calc_view(
        input_mat[, curr_col][(curr_row + 1):nrow(input_mat)],
        curr_val
      )

      view_left <- calc_view(
        rev(input_mat[curr_row, ][1:(curr_col - 1)]),
        curr_val
      )

      view_right <- calc_view(
        input_mat[curr_row, ][(curr_col + 1):ncol(input_mat)],
        curr_val
      )

      view_up * view_down * view_left * view_right
    }
  }
)

message("Max view dist: ", max(view_dist))
