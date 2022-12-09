input <- readr::read_table(
  # "day9/input",
  "day9/test_input2",
  col_names = c(
    "heading",
    "distance"
  )
) |>
  dplyr::mutate(
    axis_change = dplyr::case_when(
      heading %in% c("U", "D") ~ "x",
      TRUE ~ "y"
    ),
    signed_change = dplyr::case_when(
      heading %in% c("U", "R") ~ distance,
      TRUE ~ -distance
    )
  )

calc_path <- function(axis, amount, start_pos) {
  end_coords <- switch(axis,
    y = c(start_pos[[1]], start_pos[[2]] + amount),
    x = c(start_pos[[1]] + amount, start_pos[[2]])
  )

  if (axis == "y") {
    purrr::map(
      start_pos[[2]]:end_coords[[2]],
      ~ c(start_pos[[1]], .x)
    )
  } else {
    purrr::map(
      start_pos[[1]]:end_coords[[1]],
      ~ c(.x, start_pos[[2]])
    )
  }
}

calc_tail_path <- function(head_pos, start_pos, direction) {
  current_pos <- start_pos

  purrr::map(
    head_pos,
    ~ {
      tail_end <- current_pos

      if (!all(abs(.x - current_pos) <= 1)) {
        x_delta <- .x[[1]] - current_pos[[1]]
        y_delta <- .x[[2]] - current_pos[[2]]
        
        tail_end <- switch(
          direction,
          "U" = c(.x[[1]] - 1, .x[[2]]),
          "D" = c(.x[[1]] + 1, .x[[2]]),
          "R" = c(.x[[1]], .x[[2]] - 1),
          "L" = c(.x[[1]], .x[[2]] + 1)
        )
        
        # if (x_delta > 1 & direction == "U") {
        #   # Up
        #   tail_end <- c(.x[[1]] - 1, .x[[2]])
        # } else if (x_delta < -1 & direction == "D") {
        #   # Down
        #   tail_end <- c(.x[[1]] + 1, .x[[2]])
        # }
        # 
        # if (y_delta > 1 & direction == "R") {
        #   # Right
        #   tail_end <- c(.x[[1]], .x[[2]] - 1)
        # } else if (y_delta < -1 & direction == "L") {
        #   # Left
        #   tail_end <- c(.x[[1]], .x[[2]] + 1)
        # }

        current_pos <<- tail_end

        tail_end
      }

      tail_end
    }
  )
}

head_pos <- c(50, 50)

head_path <- purrr::map(
  1:nrow(input),
  ~ {
    path_out <- calc_path(
      input$axis_change[[.x]],
      input$signed_change[[.x]],
      head_pos
    )

    head_pos <<- path_out[[length(path_out)]]

    path_out
  }
)

# Part 1
tail_pos <- c(50, 50)

first_tail_path <- purrr::map(
  seq_along(head_path),
  ~ {
    path_iter <- calc_tail_path(head_path[[.x]], tail_pos, input$heading[[.x]])
    tail_pos <<- path_iter[[length(path_iter)]]
    path_iter
  }
)

# 6023
message("Tail visited ", length(unique(purrr::flatten(first_tail_path))), " positions")

# Part 2
all_tail_paths <- list(
  first_tail_path
)

purrr::walk(
  2:9,
  function(tail_count) {
    tail_pos_iter <- c(50, 50)

    all_tail_paths[[tail_count]] <<- purrr::map(
      seq_along(all_tail_paths[[tail_count - 1]]),
      ~ {
        path_iter <- calc_tail_path(
          all_tail_paths[[tail_count - 1]][[.x]], 
          tail_pos_iter,
          input$heading[[.x]]
          )
        
        tail_pos_iter <<- path_iter[[length(path_iter)]]
        
        path_iter
      }
    )
  }
)

# 2529 too low
length(unique(purrr::flatten(all_tail_paths[[9]])))
