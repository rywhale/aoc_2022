input <- readr::read_lines("day1/input")

# Part 1
cal_current <- 0 
cal_max <- 0

purrr::walk(
  input,
  ~{
    
    cal_current <<- ifelse(
      .x == "",
      0,
      cal_current + as.numeric(.x)
    )
    
    cal_max <<- ifelse(
      cal_current > cal_max,
      cal_current,
      cal_max
    )
    
  }
)

message("Max calories: ", cal_max)

# Part 2
cal_current <- 0

all_cal <- purrr::map_dbl(
  input,
  ~{
    cal_current <<- ifelse(
      .x == "",
      0,
      cal_current + as.numeric(.x)
    )
  }
)

message("Total for top three: ", sum(head(sort(all_cal, decreasing = TRUE), 3)))