# 02-fixIndexNumbers:
#   Reindex original index numbers by multiplying by 100, so base year = 100 instead of 1.
#   Create index numbers for all 7 original variables, but with first year as index.

# Multiply original price indices by 100 to make 100 the base. Set the units accordingly.
CSW.national.xts[,4:7] <- CSW.national.xts[,4:7]*100
unitText <- append(rep("US$B",3),rep("2000Q2 = 100", 4))
units(CSW.national.xts) <- setNames(as.list(unitText),names(CSW.national.xts))

# Make an indexed version of the original data using the first row as the base.
CSW.national.indexed.xts <- redate_base_xts(CSW.national.xts)       # Use first row as the base of the index
CSW.national.indexed.xts <- CSW.national.indexed.xts * 100          # Use 100 instead of 1.00 for the base index.

# Set the labels for the new indexed object.
labelList <- setNames(label(CSW.national.xts, units = FALSE), names(CSW.national.indexed.xts))
label(CSW.national.indexed.xts, self = TRUE) <- label(CSW.national.xts, self = TRUE)
label(CSW.national.indexed.xts) <- labelList
unitText <- rep("1975Q1 = 100",7)
units(CSW.national.indexed.xts) <- setNames(as.list(unitText),names(CSW.national.indexed.xts))
