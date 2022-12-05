input <- readr::read_lines(
  "day5/input"
)

instructs <- input[11:length(input)]

initial_state <- readr::read_fwf(
  I(input[1:8])
)

names(initial_state) <- paste0("bin_", 1:9)

move_cargo <- function(data, source, dest, count, reverse = TRUE){
  
  cargo <- data[[source]][1:count]
  cargo <- cargo[!is.na(cargo) & cargo != ""]
  
  data[[source]] <- data[[source]][-c(1:count)]

  if(reverse){
    data[[dest]] <- c(rev(cargo), data[[dest]])
  }else{
    data[[dest]] <- c(cargo, data[[dest]])
  }
  
  data
}

# Part 1
part1_bins <- as.list(initial_state)
part1_bins <- purrr::map(part1_bins, ~ .x[!is.na(.x)])

part2_bins <- part1_bins

purrr::walk(
  instructs,
  ~{
    
    instruct_vals <- stringr::str_extract_all(.x, "\\d+") |>
      unlist() |>
      as.numeric()
    
    instruct_count <- instruct_vals[[1]]
    instruct_from <- instruct_vals[[2]]
    instruct_to <- instruct_vals[[3]]
    
    part1_bins <<- move_cargo(part1_bins, instruct_from, instruct_to, instruct_count)
    part2_bins <<- move_cargo(part2_bins, instruct_from, instruct_to, instruct_count, FALSE)
  }
)

message("Part 1: ", paste(purrr::map(part1_bins, 1), collapse = ""))
message("Part 2: ", paste(purrr::map(part2_bins, 1), collapse = ""))