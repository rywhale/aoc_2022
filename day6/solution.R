input <- readr::read_lines("day6/input")

process_packet <- function(input, packet_start, packet_length) {
  current_str <- substr(input, packet_start, packet_start + packet_length - 1)
  current_str_sp <- strsplit(current_str, "")[[1]]

  if (length(unique(current_str_sp)) == packet_length) {
    current_str
  } else {
    ""
  }
}

# Part 1
part1_strs <- purrr::map_chr(
  1:nchar(input) - 3,
  ~ process_packet(input, .x, 4)
)

message("First signal at ", min(which(part1_strs != "")))

# Part 2
part2_strs <- purrr::map_chr(
  1:nchar(input) - 13,
  ~ process_packet(input, .x, 14)
)

message("First signal at ", min(which(part2_strs != "")))
