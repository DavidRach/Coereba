test_that("Coereba_Processing returns a Summarized Experiemt", {

expect_true(length(TheBioconductor) > 0)
   
expect_s4_class(TheBioconductor, "SummarizedExperiment")
cd <- colData(TheBioconductor)
expect_s4_class(cd, "DFrame")
expect_equal(nrow(cd), 1)

ratios <- assay(TheBioconductor, "ratios")
count  <- assay(TheBioconductor, "count")

expect_s3_class(ratios, "data.frame")
expect_s3_class(count, "data.frame")

})