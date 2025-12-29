test_that("Utility_MarkerPlots returns a ggplot2 object", {


ThePlot <- Utility_MarkerPlots(data=CordOnly, myfactor="ptype",
  shape_palette = shape_ptype, fill_palette = fill_ptype,
  panel=ThePanel, XAxisLevels=c("Vd2", "CD16", "CXCR5", "HLA-DR"),
  cex=3, size =3)

  # Did it return a data.frame
  expect_true(inherits(ThePlot, "gg"))
})
