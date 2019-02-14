
tf <- read.csv(file=file.path("data", "preproc_ds1and2.csv")) #,nrows = 104)
str(tf)

tf$ACT <- paste(tf$incident_state,tf$category,tf$priority,sep = ";")

write.csv(tf, file=file.path("data", "preproc_ds1and2-actEsp.csv"))

# formatting the string
tf$ACT <- gsub(pattern = "Category ",replacement = "", x = tf$ACT)

summary(tf$incident_state)

tf$ACT <- gsub(pattern = "Active",replacement = "A", x = tf$ACT)
tf$ACT <- gsub(pattern = "Awaiting Evidence",replacement = "AE", x = tf$ACT)
tf$ACT <- gsub(pattern = "Awaiting Problem",replacement = "AP", x = tf$ACT)
tf$ACT <- gsub(pattern = "Awaiting User Info",replacement = "AU", x = tf$ACT)
tf$ACT <- gsub(pattern = "Awaiting Vendor",replacement = "AV", x = tf$ACT)
tf$ACT <- gsub(pattern = "Closed",replacement = "C", x = tf$ACT)
tf$ACT <- gsub(pattern = "New",replacement = "N", x = tf$ACT)
tf$ACT <- gsub(pattern = "Resolved",replacement = "R", x = tf$ACT)

summary(tf$priority)

tf$ACT <- gsub(pattern = "1 - Critical",replacement = "1", x = tf$ACT)


tf$ACT <- gsub(pattern = "2 - High",replacement = "2", x = tf$ACT)
tf$ACT <- gsub(pattern = "3 - Moderate",replacement = "3", x = tf$ACT)
tf$ACT <- gsub(pattern = "4 - Low",replacement = "4", x = tf$ACT)


write.csv(tf, file=file.path("data", "preproc_ds1and2-actEsp2.csv"))
