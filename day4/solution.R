input <- readr::read_csv(
  "day4/input",
  col_names = c(
    "elf_1", "elf_2"
  )
) |>
  tidyr::separate(
    elf_1,
    c("elf_1_start", "elf_1_end"),
    sep = "-",
    convert = TRUE
  ) |>
  tidyr::separate(
    elf_2,
    c("elf_2_start", "elf_2_end"),
    sep = "-",
    convert = TRUE
  )

# Part 1
part1_dat <- input |>
  dplyr::mutate(
    elves_overlap = dplyr::case_when(
      elf_1_start >= elf_2_start & elf_1_end <= elf_2_end ~ TRUE,
      elf_2_start >= elf_1_start & elf_2_end <= elf_1_end ~ TRUE,
      TRUE ~ FALSE
    )
  )

message("Total elves overlap: ", sum(part1_dat$elves_overlap))

# Part 2
part2_dat <- input |>
  dplyr::mutate(
    elves_overlap = dplyr::case_when(
      elf_1_start >= elf_2_start & elf_1_start <= elf_2_end ~ TRUE,
      elf_2_start >= elf_1_start & elf_2_start <= elf_1_end ~ TRUE,
      TRUE ~ FALSE
    )
  )

message("Total elves intersect: ", sum(part2_dat$elves_overlap))

