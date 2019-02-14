
#tf <- read.csv(file=file.path("data/test", "preproc_ds1and2.csv"))
#tf <- read.csv(file=file.path("data/test", "fold1_train.csv"))
tf <- read.csv(file=file.path("data/test", "fold1_test.csv"))


# reducing size of category field
tf$category <- gsub(pattern = "Category ",replacement = "", x = tf$category)

tf$incident_state <- gsub(pattern = "Active",replacement = "A", x = tf$incident_state)
tf$incident_state <- gsub(pattern = "Awaiting Evidence",replacement = "AE", x = tf$incident_state)
tf$incident_state <- gsub(pattern = "Awaiting Problem",replacement = "AP", x = tf$incident_state)
tf$incident_state <- gsub(pattern = "Awaiting User Info",replacement = "AU", x = tf$incident_state)
tf$incident_state <- gsub(pattern = "Awaiting Vendor",replacement = "AV", x = tf$incident_state)
tf$incident_state <- gsub(pattern = "Closed",replacement = "C", x = tf$incident_state)
tf$incident_state <- gsub(pattern = "New",replacement = "N", x = tf$incident_state)
tf$incident_state <- gsub(pattern = "Resolved",replacement = "R", x = tf$incident_state)


tf$priority <- gsub(pattern = "1 - Critical",replacement = "1", x = tf$priority)
tf$priority <- gsub(pattern = "2 - High",replacement = "2", x = tf$priority)
tf$priority <- gsub(pattern = "3 - Moderate",replacement = "3", x = tf$priority)
tf$priority <- gsub(pattern = "4 - Low",replacement = "4", x = tf$priority)


#write.csv(tf, file=file.path("data/test", "fold1_train-reduced.csv"))
write.csv(tf, file=file.path("data/test", "fold1_test-reduced.csv"))



EXP2_STEP_1(horizon=7,sel_attributes=c("incident_state", "category", "priority"),
            training_fn="test/fold1_train.csv",validation_fn="test/fold1_test.csv")


EXP2_STEP_1(horizon=7,sel_attributes=c("incident_state", "category", "priority"),
            training_fn="test/fold1_train-reduced.csv",validation_fn="test/fold1_test-reduced.csv")





