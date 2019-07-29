####################################################################
############## All common functions are placed here   ##############
####################################################################

############### Calculate Elasticity ##############################

calculate_elasticity <- function(regressed_model,data_set) {
 
   #Initialize the list
   elas_model_list <- list()

  for(coeff in 2:length(regressed_model$coefficients)) {
    coeff_mean <- sum(data_set[names(regressed_model$coefficients[coeff])]) / nrow(data_set[names(regressed_model$coefficients[coeff])])
    mean_sum_gmv <- mean(data_set$sum_gmv)
    elas_model_list[coeff-1] <- regressed_model$coefficients[coeff] * coeff_mean / mean_sum_gmv
    #elas_model_list[coeff-1] <- regressed_model$coefficients[coeff] * mean(HomeAudio$name(regressed_model$coefficients[coeff])) / mean(HomeAudio$)
  }
  temp_df <- data.frame(elas_model_list)
  
  #Flip the rows and columns
  elas_model_df <- data.frame(t(temp_df[]))
  colnames(elas_model_df) <- temp_df[,1]
  
  #Merge the elasticity values with the KPI names
  elas_model_df <- cbind(elas_model_df,names(regressed_model$coefficients[2:length(regressed_model$coefficients)]))
  
  #Set the column names
  colnames(elas_model_df) <- c("Elasticity","KPI")
  
  #Arrange the data frame in descending order of elasticity value
  elas_model_df1 <- elas_model_df[order(-elas_model_df$Elasticity),]
  
  #Return the data frame
  return(elas_model_df1)
}

############### Plot Elasticity Graph ##############################

plot_elasticity <- function(df,title)
{
  ggplot(data=df, aes(x=KPI,y=Elasticity)) + geom_bar(position="dodge",stat="identity") + coord_flip() + ggtitle(title)
}
