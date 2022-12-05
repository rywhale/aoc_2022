input <- readr::read_lines(
  "day3/input"
)

# Part 1
priority_scores <- purrr::map_dbl(
  input,
  ~ {
    compart_1 <- strsplit(substr(.x, 1, nchar(.x) / 2), "")[[1]]
    compart_2 <- strsplit(substr(.x, nchar(.x) / 2 + 1, nchar(.x)), "")[[1]]

    prio_item <- intersect(compart_1, compart_2)

    if (prio_item %in% LETTERS) {
      which(LETTERS == prio_item) + 26
    } else {
      which(letters == prio_item)
    }
  }
)

message("Total priority score: ", sum(priority_scores))

# Part 2
elf_groups <- seq(1, length(input), 3)

all_badge_scores <- purrr::map_dbl(
  elf_groups,
  ~ {
    input_sp <- strsplit(input[.x:(.x + 2)], "")

    badge <- intersect(
      input_sp[[1]],
      intersect(input_sp[[2]], input_sp[[3]])
    )
    
    if (badge %in% LETTERS) {
      which(LETTERS == badge) + 26
    } else {
      which(letters == badge)
    }
  }
)

message("Total priority score: ", sum(all_badge_scores))