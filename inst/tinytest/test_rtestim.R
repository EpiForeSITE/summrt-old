#Example data 
rtestim_obj <- readRDS(
  system.file("extdata", "rtestim_example.rds",
              package = "summrt")
)

std_rtestim <- summarize_rtestimate(rtestim_obj)

message("Check that names of EpiNow2 are correct")
checkmate::expect_names( names(std_rtestim), 
                         must.include = c("estimates", 
                                          "package", 
                                          "notes"))

message("Check that the date column is actually an integer")
expect_true(is.integer(std_rtestim$estimates$date))

message("Check that the package name is correct for EpiNow2")
expect_equal(std_rtestim$package, "EpiNow2")

message("Check that there are no NAs in median, lbs, ubs")
expect_true(all(!is.na(std_rtestim$estimates$median)))
expect_true(all(!is.na(std_rtestim$estimates$lb)))
expect_true(all(!is.na(std_rtestim$estimates$ub)))


