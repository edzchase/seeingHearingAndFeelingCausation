# timing
startTime = Sys.time()

# need for sherlock
library("rwebppl") # for webppl probabalistic programming
library("dplyr") # for pipes
library("broom") # for tidy
library("caret") # for cross-validation

# to read things in
args = commandArgs()
# REALISM
visionLeftBound = as.numeric(args[3])
visionRightBound = as.numeric(args[4])
visionStep = as.numeric(args[5])

sensoryLeftBound = as.numeric(args[6])
sensoryRightBound = as.numeric(args[7])
sensoryStep = as.numeric(args[8])

windowLeftBound = as.numeric(args[9])
windowRightBound = as.numeric(args[10])
windowStep = as.numeric(args[11])

spatialLeftBound = as.numeric(args[12])
spatialRightBound = as.numeric(args[13])
spatialStep = as.numeric(args[14])

causal_prior = 0.5
real_prior = 1
unreal_prior = 1

general_left = 0
general_right = 400

samples = 10000 #as.numeric(args[12])

velocity = 0.16 # m/s estimated
location.expW = 0

# IMPORT DATA & PREPROCESS
# check out your current working directory
getwd()
# CAUSAL DATA
# Wang et al.
wang.123.df = read.csv("relatedWork/wang_123.csv", header = TRUE) %>%
  rename(offset = delay) %>%
  mutate(offset = trunc(offset)) %>%
  mutate(studyType = ifelse(study == 1, "2D",
                     ifelse(study == 2, "3D", "3D+"))) %>%
  mutate(condition = ifelse(condition == "withCollisionSound", "sound", "vision")) %>%
  mutate(signals = ifelse(condition == "vision", 1, 2)) %>%
  mutate(study = ifelse(study == 1, "Wang 1",
                        ifelse(study == 2, "Wang 2", "Wang 3")))

# shared variables & functions
epsilon = 0.035

# logit function
logit <- function(p) {
  log(p / (1 - p))
}

# define the model
model_1234sig = "webppl/model_1234sig.wppl"
# function to run the model
runModel = function(delay, signals, realism, location, noise, window, general, 
                    causal, spatial, sample) {
  
  while(length(noise) < 5) {
    noise = append(noise, 0)
  }
  while(length(realism) < 5) {
    realism = append(realism, -1)
  }
  
  if(signals == 2){ # rearrange to make sure realism info is communicated
    if(realism[2] == -1) {
      realism[2] = realism[3]
    }
  }
  
  # location of sensory cues
  if(location == 0) { 
    cue1_time = 0
    cue2_time = 0
    cue3_time = 0
  }
  else {
    cue1_time = delay*location
    cue2_time = delay*location
    cue3_time = delay*location
  }
  
  dataToWebPPL = data.frame(num_signals = signals,
                            
                            cueV1_time = 0, cueV2_time = delay, 
                            cue1_time, cue2_time, cue3_time,
                            
                            cueVision_real = realism[1], 
                            cue1_real = realism[2], cue2_real = realism[3], cue3_real = realism[4],
                            
                            cueV1_noise = noise[1], cueV2_noise = noise[2], 
                            cue1_noise = noise[3], cue2_noise = noise[4], cue3_noise = noise[5],
                            
                            window_mean = window[1], window_sigma = window[2],
                            window_left = window[3],
                            
                            general_left = general[1], general_right = general[2],
                            
                            p_causal = causal[1], 
                            like_real = causal[2], like_unreal = causal[3],
                            
                            spatialUncertainty = spatial[1],
                            velocityAtContact = spatial[2],
                            
                            samples = sample)
  
  return(((webppl(program_file = model_1234sig,
                  data = dataToWebPPL, 
                  data_var = "dataFromR")) %>% 
            mutate(t = sum(value == TRUE)/n()) %>%
            select(t) %>%
            unique())$t)
}

# for Loop - since we are generating multiple csv's here
# value to keep count of how many files we have made
currentValue = 0
# 1: vision
for(visionValue in seq(visionLeftBound, visionRightBound, visionStep))
{
  # 2: sensory
  for(sensoryValue in seq(sensoryLeftBound, sensoryRightBound, sensoryStep))
  {
    # 3: sigma
    for(windowValue in seq(windowLeftBound, windowRightBound, windowStep))
    {
      # 4: spatial
      for(spatialValue in seq(spatialLeftBound, spatialRightBound, spatialStep))
      {
        currentValue = currentValue + 1
        
        if(currentValue > 0)
        {
            noise.expW = c(visionValue, visionValue, sensoryValue, sensoryValue, sensoryValue)
            window.expW = c(0, windowValue)
            general.expW = c(general_left, general_right)
            priors.expW = c(causal_prior, real_prior, unreal_prior)
            
            wang.1.fit.df = wang.123.df %>% filter(studyType == '2D') %>%
                rowwise() %>%
                mutate(modelPrediction = runModel(offset, signals, c(0,0,0,0,0),
                    location.expW, noise.expW, window.expW, general.expW, priors.expW,
                    c(spatialValue, velocity), samples))
                                              
            ## Refit with Linear Regression using 2-Fold Cross-Validation
            # Fitting the Model Output to the Likert Data
            # Step 1: Apply a logit transform (moves data from 0 -> 1 to -inf -> +inf)
              
            # don't want to just bind the ends, but shrink the entire data set
            wang.1.fit.df = wang.1.fit.df %>%
                mutate(modelPredictionLogit = logit(epsilon + (1 - 2 * epsilon) * modelPrediction))
            
            # Step 2: Fit to the human data via a regression (humanData ~ modelPredictionLogit)
            ctrl = trainControl(method = "cv", number = 2)
            model.wang = train(causalRatings ~ modelPredictionLogit,
                                data = wang.1.fit.df, method = "lm", trControl = ctrl)
              
            # Step 3: Calculate MSE
            rmse.wang = model.wang$results$RMSE
            intercept.wang = model.wang$finalModel$coefficients[[1]]
            slope.wang = model.wang$finalModel$coefficients[[2]]
            
            # saving results
            # make a big dataframe of the important data
            wang.1.fit.df = wang.1.fit.df %>% ungroup() %>%
                            mutate(rmse = rmse.wang) %>%
                            mutate(intercept = intercept.wang) %>%
                            mutate(slope = slope.wang)
                                
              
            # save csv with essential information
            # name is a combination of the values
            title = c("wang", "run", currentValue,
                      "n", visionValue, sensoryValue, "w", windowValue,
                      "p", real_prior, unreal_prior, "s", spatialValue)
            write.csv(wang.1.fit.df, paste("relatedWorkResults/",paste(title, collapse = "_"),".csv",sep=""), row.names=FALSE)
        }
      }
    }
  }
}
endTime = Sys.time()
print(endTime - startTime)
