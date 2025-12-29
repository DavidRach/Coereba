test_that("Coereba_GateCutoffs returns a dataframe with 1 row", {

expect_true(length(UnmixedGatingSet) > 0)

# Execute the test
TheGateCutoffs <- purrr::map(.x=UnmixedGatingSet,
   .f=Coereba_GateCutoffs, subset="root",
    sample.name=c("GROUPNAME"), desiredCols=MarkersForToday) |> bind_rows()

expect_s3_class(TheGateCutoffs, "data.frame")
expect_equal(nrow(TheGateCutoffs), 1)
})
