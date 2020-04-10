
Dataset <- read.table("C:/Users/Joyce/Downloads/oj.csv", header=TRUE, 
  sep="", na.strings="NA", dec=",", strip.white=TRUE)
summary(Dataset)
local({
  .Table <- with(Dataset, 
  table(store.brand.week.logmove.feat.price.AGE60.EDUC.ETHNIC.INCOME.HHLARGE.WORKWOM.HVAL150.SSTRDIST.SSTRVOL.CPDIST5.CPWVOL5))
  cat("\ncounts:\n")
  print(.Table)
  cat("\npercentages:\n")
  print(round(100*.Table/sum(.Table), 2))
})

