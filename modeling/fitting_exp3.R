# timing
startTime = Sys.time()

# need for sherlock
library("rwebppl") # for webppl probabalistic programming
library("dplyr") # for pipes
library("broom") # for tidy

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

real_priorLeftBound = as.numeric(args[15])
real_priorRightBound = as.numeric(args[16])
real_priorStep = as.numeric(args[17])

unreal_priorLeftBound = as.numeric(args[18])
unreal_priorRightBound = as.numeric(args[19])
unreal_priorStep = as.numeric(args[20])

general_left = 0
general_right = 400

samples = 10000#as.numeric(args[12])

velocityUnreal = 0.5 # m/s ~ constant
velocityReal = 0.4886 # m/s ~ slightly less due to drag

# IMPORT DATA & PREPROCESS
# check out your current working directory
getwd()
# CAUSAL DATA
# Study 3A&B ~ (A) realism of vision, vibration, and audio, (B) location
exp3A.df = read.csv("humanData/exp3A.csv", header = TRUE) %>% select(-X) %>% 
  mutate(study = '3A') %>% mutate(location = 0) 
exp3B.df = read.csv("humanData/exp3B.csv", header = TRUE) %>% select(-X) %>% 
  mutate(study = '3B') %>% mutate(signals = 3) %>% 
  mutate(vision = ifelse(realism == 'real', 1, 0)) %>% 
  mutate(vibration = ifelse(realism == 'real', 1, 0)) %>% 
  mutate(audio = ifelse(realism == 'real', 1, 0)) %>% 
  mutate(cueInfo = realism) %>% mutate(cueInfoBasic = realism)
# Generate study conditions for Studies 3A & 3B
exp3A.study.df = exp3A.df %>% select(-response) %>% unique()
exp3B.study.df = exp3B.df %>% select(-response) %>% unique()

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
        # 5: real_prior
        for(realValue in seq(real_priorLeftBound, real_priorRightBound, real_priorStep))
        {
          # 5: unreal_prior
          for(unrealValue in seq(unreal_priorLeftBound, unreal_priorRightBound, unreal_priorStep))
          {
            currentValue = currentValue + 1
            
            if((currentValue > 515 & unrealValue == 0.7) | (currentValue > 564 & unrealValue == 0.8) | (currentValue > 937 & unrealValue == 0.9) | (currentValue > 599 & unrealValue == 1))
            {
              noise.exp3 = c(visionValue, visionValue, sensoryValue, sensoryValue, sensoryValue)
              window.exp3 = c(0, windowValue)
              general.exp3 = c(general_left, general_right)
              priors.exp3 = c(causal_prior, realValue, unrealValue)
              
              exp3A.fit.df = exp3A.study.df %>% 
                mutate(velocity = ifelse(vision == 1, velocityReal, velocityUnreal)) %>% 
                rowwise() %>%
                mutate(modelPrediction = runModel(offset, signals, 
                                                  c(vision, audio - 1, vibration - 1),
                                                  0, noise.exp3, window.exp3, general.exp3, 
                                                  priors.exp3, c(spatialValue, velocity), samples))
              
              exp3B.fit.df = exp3B.study.df %>% 
                mutate(realValue = ifelse(realism == 'real', 1, 0)) %>% 
                mutate(velocity = ifelse(realism == 'real', velocityReal, velocityUnreal)) %>% 
                rowwise() %>% 
                mutate(modelPrediction = runModel(offset, 3,
                                                  c(realValue, realValue, realValue, realValue), 
                                                  location, 
                                                  noise.exp3, window.exp3, general.exp3, 
                                                  priors.exp3, c(spatialValue, velocity), samples))
              ## Refit with Linear Regression using 2-Fold Cross-Validation
              # Fitting the Model Output to the Likert Data
              # Step 1: Apply a logit transform (moves data from 0 -> 1 to -inf -> +inf)
              
              # merge the data with the model prediction
              exp3A.fit.human.df = merge(exp3A.fit.df, exp3A.df) %>% 
                mutate(modelPredictionLogit = logit(epsilon + (1 - 2 * epsilon) * modelPrediction)) 
              exp3B.fit.human.df = merge(exp3B.fit.df, exp3B.df) %>% select(-realValue) %>% 
                mutate(modelPredictionLogit = logit(epsilon + (1 - 2 * epsilon) * modelPrediction)) 
              # merge all the data
              # don't want to just bind the ends, but shrink the entire data set
              exp3.fit.human.df = full_join(exp3A.fit.human.df, exp3B.fit.human.df)
              
              # Step 2: Fit to the human data via a regression (humanData ~ modelPredictionLogit)
              # split into two equal, but random, groups from each of the studies
              # this ensures that we can calculate an RMSE for each data set separately
              set.seed(123)
              exp3A.indices <- sample(1:nrow(exp3A.fit.human.df), nrow(exp3A.fit.human.df)/2)
              exp3B.indices <- sample(1:nrow(exp3B.fit.human.df), nrow(exp3B.fit.human.df)/2)
              
              exp3.fit.human.half1.df = full_join(exp3A.fit.human.df[exp3A.indices, ], 
                                                  exp3B.fit.human.df[exp3B.indices, ]) 
              exp3.fit.human.half2.df = full_join(exp3A.fit.human.df[-exp3A.indices, ], 
                                                  exp3B.fit.human.df[-exp3B.indices, ])
              
              # Fit linear regression to each of these sets
              model.1 = lm(response ~ modelPredictionLogit, data = exp3.fit.human.half1.df)
              model.2 = lm(response ~ modelPredictionLogit, data = exp3.fit.human.half2.df)
              
              # Step 3: Calculate MSE
              # Calculate appropriate RMSE values from each data set
              predict.m1.half2.A = predict(model.1, 
                                           exp3.fit.human.half2.df %>% filter(study == '3A'), 
                                           se.fit = TRUE)
              predict.m1.half2.B = predict(model.1, 
                                           exp3.fit.human.half2.df %>% filter(study == '3B'), 
                                           se.fit = TRUE)
              
              predict.m2.half1.A = predict(model.2, 
                                           exp3.fit.human.half1.df %>% filter(study == '3A'), 
                                           se.fit = TRUE)
              predict.m2.half1.B = predict(model.2, 
                                           exp3.fit.human.half1.df %>% filter(study == '3B'), 
                                           se.fit = TRUE)
              
              rmse.m1.half2.A = sqrt(mean((predict.m1.half2.A$fit - 
                                             (exp3.fit.human.half2.df %>% 
                                                filter(study == '3A'))$response)^2))
              rmse.m1.half2.B = sqrt(mean((predict.m1.half2.B$fit - 
                                             (exp3.fit.human.half2.df %>% 
                                                filter(study == '3B'))$response)^2))
              
              rmse.m2.half1.A = sqrt(mean((predict.m2.half1.A$fit - 
                                             (exp3.fit.human.half1.df %>% 
                                                filter(study == '3A'))$response)^2))
              rmse.m2.half1.B = sqrt(mean((predict.m2.half1.B$fit - 
                                             (exp3.fit.human.half1.df %>% 
                                                filter(study == '3B'))$response)^2))
              
              # even though studies have different n, this treats them equally
              rmse.exp3A = (rmse.m1.half2.A + rmse.m2.half1.A)/2
              rmse.exp3B = (rmse.m1.half2.B + rmse.m2.half1.B)/2
              
              rmse.exp3 = (rmse.m1.half2.A + rmse.m1.half2.B + rmse.m2.half1.A + rmse.m2.half1.B)/4
              
              # saving results
              # make a big dataframe of the important data
              exp3.fit.df = rbind(exp3A.fit.df %>% mutate(rmse = rmse.exp3A), 
                                  exp3B.fit.df %>% mutate(rmse = rmse.exp3B) %>% 
                                    select(-realValue)) %>% 
                ungroup() %>% mutate(rmseTotal = rmse.exp3)
              
              # save csv with essential information
              # name is a combination of the values
              title = c("exp3", "run", currentValue, 
                        "n", visionValue, sensoryValue, "w", windowValue,
                        "p", realValue, unrealValue, "s", spatialValue)
              write.csv(exp3.fit.df, paste("modelResults/",paste(title, collapse = "_"),".csv",sep=""), row.names=FALSE)
            }
          }
        }
      }
    }
  }
}
endTime = Sys.time()
print(endTime - startTime)
