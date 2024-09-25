# Example data 
EpiNow2_obj <- readRDS(
  system.file("extdata", "EpiNow2_example.rds",
            package = "summrt")
)

summarize_rtestimate(EpiNow2_obj)