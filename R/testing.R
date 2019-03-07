# testing the expand algorithm

HIGH_ERROR_CONST <- 2^31

xsf <- c("s1","s2","s3","s4","s5","s6")
xis <- list(fields = c(), value = HIGH_ERROR_CONST, horiz = Inf)
xresult <- expand_state_fn(xis,xsf)

rm(testing_expand_state_fn)

expand_state_fn <- function(astate, asearch_fields)
{
   lstate_fields <- astate$fields
   lavailable_fields <- asearch_fields[!(asearch_fields %in% lstate_fields)]
   lexpanded_state_list <- list()
   if (length(lavailable_fields) > 0)
   {
      for (i in 1:length(lavailable_fields))
      {
         lstate <- list(fields=c(lstate_fields, lavailable_fields[i]), value=0, horiz = 1)
         lexpanded_state_list[[i]] <- lstate
      }
   }
   return(lexpanded_state_list)
}

belong_to_list2 <- function(astate_list, astate)
{
   #astate_list <- open_list
   #astate <- expanded_state
   #rm(astate_list, astate)
   #state_exists <- F
   scount <- 1
   lstate_fields <- as.set(astate$fields)
   while (scount <= length(astate_list))
   {
      lstate_list_fields <- as.set(astate_list[[scount]]$fields)
      if (gset_is_equal(lstate_list_fields, lstate_fields))
      {
         break
      }
      scount <- scount + 1
   }
   if (scount > length(astate_list))
      scount <- 0
   return(scount)
}

put_state <- function(astate_list, astate)
{
   lstate_list <- astate_list
   Result = length(lstate_list) + 1
   # print(Result)
   lstate_list[[Result]] <- astate
   eval.parent(substitute(astate_list<-lstate_list))
   return(Result)
}

expanded_state_list <- NULL
expanded_state_list_tmp <- NULL
MAX_K_EXP_COUNT <- 5
k_exp_count <- 1
v_state <- list(fields = c(), value = HIGH_ERROR_CONST, horiz = Inf)
search_fields <- c("s1","s2","s3","s4","s5")
horiz_array <- c(1,2,3,4,5)

while ((k_exp_count <= MAX_K_EXP_COUNT))
{
   if (k_exp_count <= MAX_K_EXP_COUNT) {
      expanded_state_list <- expand_state_fn(v_state, search_fields)
      expanded_state_list_tmp <- list()
      for (expanded_state in expanded_state_list)
      {
            put_state(expanded_state_list_tmp, expanded_state)
      }
      expanded_state_list <- expanded_state_list_tmp

      #ptime <- system.time({
      if  (length(expanded_state_list) > 0)
      {
         lcount <- 1
         expanded_state_list_tmp <- list()
         for (i in 1:length(expanded_state_list))
         {
            for (j in 1:length(horiz_array))
            {
               lstate_tmp <- list(fields=expanded_state_list[[i]]$fields, horiz = horiz_array[j])
               expanded_state_list_tmp[[lcount]] <- lstate_tmp
               lcount <- lcount + 1
            }
         }
      }
   }
   k_exp_count <- k_exp_count + 1
}




rm(expanded_state,expanded_state_list,expanded_state_list_tmp, lstate_tmp,v_state,xis,xresult)



# hill climbing in r

library(rpart)

library(FSelector)

data(iris)

# exemplo da biblioteca
evaluator <- function(subset) {
   #k-fold cross validation
   k <- 5
   splits <- runif(nrow(iris))
   results = sapply(1:k, function(i) {
      test.idx <- (splits >= (i - 1) / k) & (splits < i / k)
      train.idx <- !test.idx
      test <- iris[test.idx, , drop=FALSE]
      train <- iris[train.idx, , drop=FALSE]
      tree <- rpart(as.simple.formula(subset, "Species"), train)
      error.rate = sum(test$Species != predict(tree, test, type="c")) / nrow(test)
      return(1 - error.rate)
   })
   print(subset)
   print(mean(results))
   return(mean(results))
}

subset <- hill.climbing.search(names(iris)[-5], evaluator)
f <- as.simple.formula(subset, "Species")
print(f)


as.simple.formula(iris, "Species")


attrib <- c("s1","s2","s3","s4","s5","s6")
attrib_values <- c("s1"=1,"s2"=2,"s3"=3,"s4"=5,"s5"=7,"s6"=11)

attrib_eval <- function(at)
{
   if (length(at)==1) {
      return(attrib_values[at[1]])
   }
   soma <- 0
   for(i in 1:length(at))
   {
      soma <- soma + attrib_values[at[i]]
   }
   return((soma*2/length(at)))
}

attrib_eval(c("s4","s2"))

subset <- hill.climbing.search(attrib, attrib_eval)
f <- as.simple.formula(subset,"m")
print(f)

subset <- best.first.search(attrib, attrib_eval)
f <- as.simple.formula(subset,"m")
print(f)


MAPE(1, 1); MAPE(2, 1); MAPE(3, 1); MAPE(0, 1); MAPE(1.1, 1)

RMSPE(1, 1); RMSPE(2, 1); RMSPE(3, 1); RMSPE(0, 1); RMSPE(1.1, 1)
