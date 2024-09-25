# Example data 
EpiLPS_obj <- readRDS(
  system.file("extdata", "EpiLPS_example.rds",
              package = "summrt")
)

std_epilps <- summarize_rtestimate(EpiLPS_obj)

message("Check that names of EpiLPS are correct")
checkmate::expect_names( names(std_epinow2), 
                         must.include = c("estimates", 
                                          "package", 
                                          "notes"))

message("Check that the date column is actually an integer")
expect_true(is.integer(std_epinow2$estimates$date))

message("Check that the package name is correct for EpiLPS")
expect_equal(std_epinow2$package, "EpiLPS")

message("Check that there are no NAs in median, lbs, ubs")
expect_true(all(!is.na(std_epinow2$estimates$median)))
expect_true(all(!is.na(std_epinow2$estimates$lb)))
expect_true(all(!is.na(std_epinow2$estimates$ub)))