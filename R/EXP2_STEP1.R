# objetivo: realizar a busca da melhor combinação de atributos
#           usando os resultados de MAPE para avaliar


# make sure that all the important constants are set before start
TRACE_ID_COLUMN_NAME <- "number"
STATUS_COLUMN_NAME <- "incident_state"
EVENT_TIMESTAMP_COLUMN_NAME <- "updated_at"
CLOSED_STATUS_VALUE <- "Closed"
REMOVE_LONGER_CASES <- FALSE
INCLUDE_SOJOURN_IN_PREDICTION <- FALSE

LAST_RUN_MODEL <- NULL
LAST_RUN_PREDICT <- NULL

# these were the best horizons selected according to the tests using the
# incident_state, category and priority (expert selection)
selected_horizon <- c(5,6,7)

#selected_attributes <- c("incident_state", "category", "priority") # expert

selected_attributes <- c(
"active",
"assigned_to",
"assignment_group",
"caller_id",
"category",
"incident_state",
"knowledge",
"location",
"made_sla",
"opened_by",
"problem_id",
"reassignment_count",
"u_symptom",
"subcategory",
"u_priority_confirmation")

# resolved_by não seria preenchido só no final? se for, não deveria estar sendo usado
# solução: removido o resolved_by da lista e substituído pelo u_symptom

# res <- NULL
# for(i in 1:length(selected_attributes))
# {
#    for(j in 1:length(selected_horizon))
#    {
#       res[i,j] <- EXP2_STEP_1(horizon=selected_horizon[j],sel_attributes=selected_attributes_p1[i])
#    }
# }

res_01_5 <- EXP2_STEP_1(horizon=5,sel_attributes="active")

res_01_6 <- EXP2_STEP_1(horizon=6,sel_attributes="active")

res_01_7 <- EXP2_STEP_1(horizon=7,sel_attributes="active")


res_05_5 <- EXP2_STEP_1(horizon=5,sel_attributes="category")

res_05_6 <- EXP2_STEP_1(horizon=6,sel_attributes="category")

res_05_7 <- EXP2_STEP_1(horizon=7,sel_attributes="category")


x <- EXP2_STEP_1(horizon=5,sel_attributes=c("assigned_to","subcategory"))


EXECUTION_DESCRIPTION <- " Execução com horizontes selecionados, em 20K (A+B) validado contra 4.9K (C)"

exp1_final_h6 <- EXP2_STEP_1(horizon=6,sel_attributes=c("incident_state", "category", "priority"))
exp1_final_h7 <- EXP2_STEP_1(horizon=7,sel_attributes=c("incident_state", "category", "priority"))


REMOVE_LONGER_CASES <- TRUE

exp1_final_h6_rl <- EXP2_STEP_1(horizon=6,sel_attributes=c("incident_state", "category", "priority"))
exp1_final_h7_rl <- EXP2_STEP_1(horizon=7,sel_attributes=c("incident_state", "category", "priority"))


REMOVE_LONGER_CASES <- FALSE
INCLUDE_SOJOURN_IN_PREDICTION <- TRUE

exp1_final_h6_c3 <- EXP2_STEP_1(horizon=6,sel_attributes=c("incident_state", "category", "priority"))
exp1_final_h7_c3 <- EXP2_STEP_1(horizon=7,sel_attributes=c("incident_state", "category", "priority"))

REMOVE_LONGER_CASES <- FALSE
INCLUDE_SOJOURN_IN_PREDICTION <- FALSE

exp1_final_h8 <- EXP2_STEP_1(horizon=8,sel_attributes=c("incident_state", "category", "priority"))
exp1_final_h9 <- EXP2_STEP_1(horizon=9,sel_attributes=c("incident_state", "category", "priority"))


EXP2_STEP_1 <- function(horizon=selected_horizon, sel_attributes=selected_attributes,
                        training_fn="preproc_ds1and2.csv",validation_fn="preproc_ds3.csv")
{
   generate_log(" ************* Initiating EXP 1 STEP 5 *************", 1)
   generate_log(EXECUTION_DESCRIPTION)
   generate_log(paste(" REMOVE_LONGER_CASES == ",REMOVE_LONGER_CASES))
   generate_log(paste(" INCLUDE_SOJOURN_IN_PREDICTION == ",INCLUDE_SOJOURN_IN_PREDICTION))
   generate_log(paste("Training file: ",training_fn,"Validation file:",validation_fn))

   startTime <- Sys.time()

   # the base name for the files that will store all the results
   statsFile <- paste("results_EXP1_STEP5_",format(startTime, "%Y%m%d-%H%M"),sep="")

   model <- NULL
   predict <- NULL
   eval_stats_arr <- NULL

   trainingFold <- read.csv(file=file.path("data", training_fn)) #,nrows = 104)

   #option to remove from the training the outliers with elapsed time much bigger
   if ( REMOVE_LONGER_CASES == TRUE ) {
      q <- quantile(trainingFold$elapsed_stc,0.99)
      onePerc <- trainingFold[trainingFold$elapsed_stc > q,c("number","elapsed_stc")]
      onePercDist <- distinct(onePerc,onePerc$number)
      colnames(onePercDist) <- c("number")
      generate_log(paste("Removing ",nrow(onePercDist)," traces that have elapsed times bigger than [",q,"] seconds"))
      '%ni%' <- Negate('%in%')
      trainingFold <- trainingFold[trainingFold$number %ni% onePercDist$number,]
   }

   rfn <- file.path("data/test",paste(statsFile,"_pred.csv",sep=""))

   # builds the transition system
   model <- build_ats(trainingFold,horizon,sel_attributes)
   # anotates the transition system
   training_stats <- annotate_model(trainingFold, rfn, "T", 0, horizon)

   # prediction over the validation data set
   testingFold <- read.csv(file=file.path("data", validation_fn)) #,nrows = 104)

   # builds the transition system for teh testing fold
   predict <- build_prediction(testingFold,model)
   validation_stats <- annotate_model(testingFold, rfn, "V", 0, horizon)

   eval_stats_arr <- rbind(training_stats, validation_stats)


   eval_stats_df1 <- data.frame(eval_stats_arr)

   #grp_eval_stats_tp <- summarise_all (
   #   group_by(eval_stats_df1, horizon, fold),
   #   funs(mean, sd), na.rm = TRUE)

   #sfilen <- file.path("data/test",paste(statsFile,"_GRSTATS.csv",sep=""))
   #write.table(grp_eval_stats_tp, file=sfilen, row.names=FALSE, col.names = TRUE, sep=";", dec=",")

   string_attrib <- paste(unlist(sel_attributes),collapse=",")
   eval_stats_df1 <- cbind(start=format(startTime, "%Y-%m-%d %H:%M:%S"),end=format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
                           at="laptop", attributes=string_attrib,eval_stats_df1)

   sfilen <- file.path("data/test",paste(statsFile,"_STATS.csv",sep=""))
   write.table(eval_stats_df1, file=sfilen, row.names=FALSE, col.names = TRUE, sep=";", dec=",")

   generate_log(paste("Stat file generated: [",statsFile,"_STATS.csv]",sep=""))
   generate_log("Step completed successfully.",2)

   return(eval_stats_df1)

}
