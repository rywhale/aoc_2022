input <- readr::read_table(
  "day2/input",
  col_names = c("chud", "me")
)

# Part 1
lookup_wins <- matrix(
  c(
    3, 0, 6,
    6, 3, 0,
    0, 6, 3
  ),
  ncol = 3, 
  nrow = 3,
  dimnames = list(
    c("A", "B", "C"),
    c("X", "Y", "Z")
  )
)

part1 <- input |>
  dplyr::mutate(
    shape_score = dplyr::case_when(
      me == "X" ~ 1,
      me == "Y" ~ 2,
      TRUE ~ 3
    ),
    total_score = purrr::map2_dbl(chud, me, ~lookup_wins[[.x, .y]]) + shape_score
  )

message("Total: ", sum(part1$total_score))

# Part 2
shape_lookup <- matrix(
  c(
    3, 1, 2,
    1, 2, 3,
    2, 3, 1
    ),
  ncol = 3, 
  nrow = 3,
  dimnames = list(
    c("A", "B", "C"),
    c("X", "Y", "Z")
  )
)

part2 <- input |> 
  dplyr::mutate(
    win_score = dplyr::case_when(
      me == "X" ~ 0,
      me == "Y" ~ 3,
      me == "Z" ~ 6
    ),
    total_score = purrr::map2_dbl(chud, me, ~shape_lookup[[.x, .y]]) + win_score
  )

message("Total: ", sum(part2$total_score))