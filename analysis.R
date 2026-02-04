
library(openxlsx)
df <- read.xlsx(xlsxFile = "E:\\RESEARCH\\Field of Fortunes\\output_completed_final.xlsx")

clean_string <- function(s) {
  parts <- strsplit(s, ",")[[1]]
  parts <- gsub('"', '', parts) 
  parts <- parts[parts != "NA"]
  paste(parts, collapse = ",")
}

#game plays
sum(apply(table(df$Player,df$Play),2,function(x) x != 0))

#mean game plays
sum(apply(table(df$Player,df$Play),2,function(x) x != 0))/max(df$Player)
plays <- table(df$Player,df$Play)

#how many people played 2 or more times?
plays_per_player <- apply(plays,1,function(x) sum(x > 0))
sum(plays_per_player>1)
sum(plays_per_player>7)

#the average number of turns per player
mean(df[df$TotalTurns==df$Turn,]$Turn)

#the average score per player
mean(df[df$TotalTurns==df$Turn,]$Bank)
median(df[df$TotalTurns==df$Turn,]$Bank)

games <- df[df$TotalTurns==df$Turn,]

df$complexity <- gsub("[","", df$Forecast,fixed=TRUE)
df$complexity <- gsub("]","", df$complexity,fixed=TRUE)
df$complexity <- vapply(df$complexity, clean_string, character(1))
df$complexity <- strsplit(df$complexity,",")
df$complexity <- unlist(lapply(df$complexity,length))

#how many players sought instructions at least one time in a game
temp <- table(df$Player,df$ClickedInstructions)
sum(temp[,2] != 0)
38/54

#how many crop planting decisions in total
library(stringr)
df$crop_decisions <- str_count(df$AttemptedCrops,",")+1
sum(df$crop_decisions)

#----------------------------------------------------
#interesting, but not an hypothesis of the research...
#is there a trend over time?
#would need to have  game-level clustering too....not super useful?
#specifically,
df$instructions <- ifelse(df$ClickedInstructions=="Yes",1,0)
out <- lm(df$Bank ~ df$Turn + df$instructions)
summary(out)
library(lme4)
out <- lmer(df$Bank ~ df$Turn + df$instructions + (1+df$Turn|df$Player))
summary(out)
#----------------------------------------------------

#is there a correlation between the number of turns, and the score?
score <- aggregate(df$TotalTurns,by=list(df$Player),FUN=max)
summary(score$x)

temp <- df[df$Play==1 & df$Turn==1,]
mean(temp$ForecastCost[temp$BuyForecast=="F"])
mean(temp$ForecastCost[temp$BuyForecast=="T"])

#---------------------
#Hypothesis 1: Hypothesis 1: In turn 1, players who do not buy a
#forecast should attempt to plant crops in all 8 available fields.
#---------------------

df_turn <- df[df$Turn==1,]
df_turn$allplanted <- ifelse(nchar(df_turn$AttemptedCrops)==63,1,0)
df_turn$BuyForecast <- ifelse(df_turn$BuyForecast=='T',1,0)
table(df_turn$allplanted,df_turn$BuyForecast)
chisq.test(df_turn$allplanted,df_turn$BuyForecast)

library(lme4)
library(performance)

model_glm <- glm(df_turn$allplanted~df_turn$BuyForecast,family=binomial)
summary(model_glm)
mu <- model_glm$coefficients[1]*1 + model_glm$coefficients[2]*1
p_11 <- 1/(1+exp(-mu))
mu <- model_glm$coefficients[1]*1
p_10 <- 1/(1+exp(-mu))

#Table 2---------------------------------------------------------------------------
#random effect is the player
model_glmma <- glmer(allplanted~BuyForecast+instructions+
                      (1|Player),family=binomial,data=df_turn)
summary(model_glmma)
icc(model_glmma)

model_glmmb <- glmer(allplanted~BuyForecast+instructions+BuyForecast*instructions+(1|Player),family=binomial,data=df_turn)
AIC(model_glmma,model_glmmb)#b has a smaller AIC, we use it
model_glmm <- model_glmmb
summary(model_glmm)
icc(model_glmm)
#ORs calculated with calculator
#----------------------------------------------------------------------------------

mu <- model_glmm@beta[1]*1 + model_glmm@beta[2]*1
p_11 <- 1/(1+exp(-mu))
mu <- model_glmm@beta[1]*1
p_10 <- 1/(1+exp(-mu))

intercept <- model_glmm@beta[1]
forecast_coeff <- model_glmm@beta[2]
random_intercept_sd <- 1.785
forecast_value <- 1
logit_fixed <- intercept + forecast_coeff * forecast_value
logit_zero <- logit_fixed  
probability_zero <- 1 / (1 + exp(-logit_zero))
logit_plus1SD <- logit_fixed + random_intercept_sd
probability_plus1SD <- 1 / (1 + exp(-logit_plus1SD))
logit_minus1SD <- logit_fixed - random_intercept_sd
probability_minus1SD <- 1 / (1 + exp(-logit_minus1SD))

#---------------------
#Hypothesis 2: For all turns in which a player buys a forecast, they should plant
#the most valuable crops in the fields that have the highest values of p
#---------------------

sum(df[df$Turn==1,"BuyForecast"]=="T")/153
sum(df[df$Turn==1,"BuyForecast"]=="F")/153

crops <- c("corn","soy","wheat","barley","oats","lentils","canola","rye")
corp_value <- c(100,150,200,200,100,300,300,100)

df_question2 <- df[df$BuyForecast=="T",]
nrow(df_question2)/711

#write.xlsx(df_question2,file = "E:\\RESEARCH\\Field of Fortunes\\question2_data_test.xlsx")

#these are TURNS not games.  about 50% of turns, 71% of games
df3 <- read.csv(file="E:\\RESEARCH\\Field of Fortunes\\completed_optimal_plantings.csv")

df_question2$mistakes <- df3$Misplaced.count

df_question2 <- df_question2[,c("Player","Play","Turn","mistakes","Forecast","ClickedInstructions")]

#what fraction of turns (with forecasts) included a mistake?
sum(df_question2$mistakes>0)
sum(df_question2$mistakes<=0)

sum(df_question2$mistakes>0 & df_question2$ClickedInstructions=="Yes")
sum(df_question2$mistakes<=0 & df_question2$ClickedInstructions=="Yes")

sum(df_question2$mistakes>0 & df_question2$ClickedInstructions=="No")
sum(df_question2$mistakes<=0 & df_question2$ClickedInstructions=="No")

out <- aggregate(df_question2$mistakes,by=list(df_question2$Player,df_question2$Play),FUN=max)
table(df_question2$mistakes)

#combine the data so I can model it

df_question2$V3 <- df3[,4]
df_question2$V4 <- df3[,5]

#expand the decisions
#must parse the string and then expand to long format...should have done in python...
reorganize_data <- function(df,v1_col="V1",v2_col="V2",v3_col="V3",v4_col="V4",v5_col="V5") {
  parse_to_numvec <- function(x) {
    if (is.na(x)||trimws(x)=="") {
      return(NA)
    }
    x_clean <- gsub("\\[|\\]","", as.character(x))
    splitted <- strsplit(x_clean, ",")[[1]]
    splitted <- trimws(splitted)
    splitted[splitted == "None"] <- NA
    splitted[splitted == ""] <- NA
    
    numvec <- suppressWarnings(as.numeric(splitted))
    if (length(numvec) < 1 || all(is.na(numvec))) {
      return(NA)
    }
    
    return(numvec)
  }
  df[[v3_col]] <- lapply(df[[v3_col]], parse_to_numvec)
  df[[v4_col]] <- lapply(df[[v4_col]], parse_to_numvec)
  row_lengths <- sapply(seq_len(nrow(df)), function(i) {
    len_v3 <- if (all(is.na(df[[v3_col]][[i]]))) 0 else length(df[[v3_col]][[i]])
    len_v4 <- if (all(is.na(df[[v4_col]][[i]]))) 0 else length(df[[v4_col]][[i]])
    max(len_v3, len_v4)
  })
  
  expanded_df_list <- lapply(seq_len(nrow(df)), function(i) {
    num_rows <- row_lengths[i]
    if (num_rows < 1) {
      num_rows <- 1
    }
    v3_expanded <- rep(NA_real_, num_rows)
    v4_expanded <- rep(NA_real_, num_rows)
    v3_vals <- df[[v3_col]][[i]]
    v4_vals <- df[[v4_col]][[i]]
    if (!all(is.na(v3_vals)) && length(v3_vals) > 0) {
      v3_expanded[seq_along(v3_vals)] <- v3_vals
    }
    if (!all(is.na(v4_vals)) && length(v4_vals) > 0) {
      v4_expanded[seq_along(v4_vals)] <- v4_vals
    }
    data.frame(
      V1 = rep(df[[v1_col]][i], num_rows),
      V2 = rep(df[[v2_col]][i], num_rows),
      V3 = as.numeric(v3_expanded),
      V4 = as.numeric(v4_expanded),
      V5 = rep(df[[v5_col]][i], num_rows)
    )
  })
  expanded_df <- do.call(rbind, expanded_df_list)
  rownames(expanded_df) <- NULL
  return(expanded_df)
}

df_question2[df_question2 == "None"] <- NA
df_correlations <- reorganize_data(
  df = df_question2,
  v1_col = "Player",
  v2_col = "Play",
  v3_col = "V3",
  v4_col = "V4",
  v5_col = "Turn"
)

df_correlations <- df_correlations[!is.na(df_correlations$V3),]
df_correlations <- df_correlations[!is.na(df_correlations$V4),]

#the turn va

df_correlations$mismatch <- ifelse(df_correlations$V3==df_correlations$V4,0,1)
df_correlations$Player <- df_correlations$V1
df_correlations$Play <- df_correlations$V2
df_correlations$Turn <- df_correlations$V5

df_correlations <- df_correlations[,c("Player","Play","mismatch","Turn")]

df_correlations <- merge(df_correlations,df[,c("Player","Play","ForecastCost","Turn","complexity","instructions")],
                         by=c("Player","Play","Turn"),all.x=TRUE)

df_correlations$Player <- as.factor(df_correlations$Player)
df_correlations$Play <- as.factor(df_correlations$Play)
df_correlations$order <- as.numeric(df_correlations$Play)

#nested structure, non scaled
#model_glmm_both <- glmer(mismatch~ForecastCost+order+complexity+(1|Player/Play),data=df_correlations,family=binomial)
#summary(model_glmm_both)

#because player-level effects (above) does not work in the model, focus on game-player
#model_glmm_play_player <- glmer(mismatch~ForecastCost+complexity+instructions+(1|Player:Play),data=df_correlations,family=binomial)
#summary(model_glmm_play_player)
#icc(model_glmm_play_player)
#anova(model_glmm_both,model_glmm_play_player)

#here include games as a fixed rather than random effect
#Play fixed effect = average learning across attempts
#(1|Player) = stable player differences
#(1|Player:Play) = correlation among decisions within the same playthrough
#as.numeric(Play) is the fixed effect recommended by the reviewer
model_glmm_play_player <- glmer(mismatch~ForecastCost+complexity+instructions+as.numeric(Play)+(1|Player)+(1|Player:Play),data=df_correlations,family=binomial)
summary(model_glmm_play_player)
icc(model_glmm_play_player)

#error rates by Play order
table(df_correlations$Play,df_correlations$mismatch)[,2]/
table(df_correlations$Play,df_correlations$mismatch)[,1]




#this dos not meet inclusion criteria
#model_glmm_play_player <- glmer(mismatch~ForecastCost+order+ForecastCost*order+complexity+(1|Player:Play),data=df_correlations,family=binomial)
#summary(model_glmm_play_player)

#scaling to deal with problems
df_correlations$ForecastCost_ctr <- scale(df_correlations$ForecastCost,scale=FALSE)
df_correlations$order_ctr <- scale(df_correlations$order,scale=FALSE)
df_correlations$turn_ctr <- scale(df_correlations$Turn,scale=FALSE)
df_correlations$complexity_ctr <- scale(df_correlations$complexity,scale=FALSE)

model_glmm_play_player <- glmer(mismatch~ForecastCost_ctr+order_ctr+complexity_ctr+(1|Player:Play),data=df_correlations,family=binomial)
summary(model_glmm_play_player)


#visualize original (non-standardized) values
mean_forecast <- mean(df_correlations$ForecastCost)
mean_complexity <- mean(df_correlations$complexity)
mean_order <- mean(df_correlations$order)
cost_levels <- c(25,50,75)
names(cost_levels) <- c("Low","Medium","High")

complexity_vals <- seq(min(df_correlations$complexity),max(df_correlations$complexity),length.out=100)
pred_data <- expand.grid(ForecastCost=cost_levels,complexity=complexity_vals)
pred_data$ForecastCost_ctr <- pred_data$ForecastCost-mean_forecast
pred_data$complexity_ctr <- pred_data$complexity-mean_complexity
pred_data$order_ctr <- 0
# Predict using fixed effects only
pred_data$pred <- predict(model_glmm_play_player,newdata=pred_data,type="response",re.form=NA)

#in case I want more lines, loop
plot(NA, NA,xlim=range(1,8),ylim=c(0,0.2),xlab = "Complexity (Decisions per Turn)",
     ylab="Predicted Probability of Error")
cols <- c("blue","black","red")
style <- c(1,4,21)
line_labels <- names(cost_levels)

for (i in 1:3) {
  idx <- pred_data$ForecastCost == cost_levels[i]
  lines(pred_data$complexity[idx],
        pred_data$pred[idx],
        col = cols[i],
        lty = style[i],
        lwd = 3)
}
legend("topleft",legend=paste("Forecast Cost:",round(cost_levels,1)),
       col=cols,lwd=3,lty=style)

#---------------------
#Hypothesis 3: do players only buy forecasts when the TVOF is larger than the price
#of the forecast?
#---------------------
library(stringr)

df4 <- read.xlsx(xlsxFile = "E:\\RESEARCH\\Field of Fortunes\\processed_crop_values.xlsx")
df4$row_id <- seq_len(nrow(df4))
#delete the last row
#df4 <- df4[-nrow(df4),]
df4 <- merge(df4,df[,c("Player","Play","ForecastCost","Turn","BuyForecast","instructions")],
      by=c("Player","Play","Turn"),all.x=TRUE)
table(table(df4$row_id))
#manually elete teh doubles
dups <- as.integer(names(which(table(df4$row_id) > 1)))

df4 <- df4[-c(193,195,196,198,200,202,204,206,209,211),]

df4$remainingCrops <- str_count(df4$currentCrop,',')+1
df4$TVOF <- df4$cropValue/12

df4$NVOF <- df4$TVOF-df4$ForecastCost
df4$buy <- ifelse(df4$BuyForecast=="T",1,0)
df4$NVOF_ctr <- scale(df4$NVOF,scale=TRUE)
df4$NVOF_sq_ctr <- df4$NVOF_ctr*df4$NVOF_ctr

#average NVOF for buyers
mean(df4$NVOF[df4$BuyForecast=="T"])
mean(df4$NVOF[df4$BuyForecast=="F"])

m1 <- df4$NVOF[df4$BuyForecast=="T"]
m2 <- df4$NVOF[df4$BuyForecast=="F"]
t.test(m1,m2)


model_glmm_play_player_buy <- glmer(buy~NVOF_ctr+NVOF_sq_ctr+instructions+(1|Player:Play),
                                data=df4,family=binomial)
summary(model_glmm_play_player_buy)
icc(model_glmm_play_player_buy)
predicted <- model_glmm_play_player_buy@resp$mu[df4$instructions==1]

pred_fixedonly <- predict(model_glmm_play_player_buy,newdata=df4[df4$instructions==1,],type="response",re.form=NA)
x <- scale(df4$NVOF[df4$instructions==1],scale=TRUE)

ord <- order(x) 
x <- x[ord]
pred_fixedonly <- pred_fixedonly[ord]


sd(df4$NVOF)
median(df4$NVOF)

y_theory <- ifelse(x<0,0,ifelse(x>0,1,0.5))


plot(x,pred_fixedonly,type="l",col="red",lty=1,lwd=2,ylim=c(0,1))
lines(x, y_theory, col="black", lty=2, lwd=2)
#post-production in ink scape since this graphic is not great
legend("bottomright", 
legend=c("Model predicted", "Theoretical"),col=c("red", "black"),lty=c(1,2),lwd=2,cex=1.2)

