# timing
startTime = Sys.time()

# need for sherlock
library("rwebppl") # for webppl probabalistic programming
library("dplyr") # for pipes
library("broom") # for tidy

# to read things in
args = commandArgs()
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

general_left = -84#as.numeric(args[15])
general_right = 600#as.numeric(args[16])

samples = 10000#as.numeric(args[10])

velocity = 0.5 # m/s

# IMPORT DATA & PREPROCESS
# check out your current working directory
getwd()
# CAUSAL DATA
# Study 1 ~ negative & positive offset, vision & kinesthetic
exp1.df = read.csv("humanData/exp1.csv", header = TRUE) %>% select(-X) %>% 
  mutate(study = 1)
# Study 2 ~ positive offset, uni/bi/tri/quad modal signals
exp2.df = read.csv("humanData/exp2.csv", header = TRUE) %>% select(-X) %>% 
  mutate(study = 2)
# Generate study conditions for Studies 1 & 2
exp1.study.df = exp1.df %>% group_by(signals, block, offset) %>% 
  mutate(numOnes = sum(response)) %>% 
  mutate(numZeros = n() - sum(response)) %>% 
  mutate(total = numOnes + numZeros) %>% 
  select(signals, block, offset, numOnes, numZeros, total) %>%
  unique()
exp2.study.df = exp2.df %>% group_by(signals, block, offset) %>% 
  mutate(numOnes = sum(response)) %>% 
  mutate(numZeros = n() - sum(response)) %>% 
  mutate(total = numOnes + numZeros) %>% 
  select(signals, block, offset, numOnes, numZeros, total) %>%
  unique()

# shared variables & functions
epsilon = 0.035

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

        if(currentValue > 180)
        {
  				noise.exp12 = c(visionValue, visionValue, sensoryValue, sensoryValue, sensoryValue)
  				window.exp12 = c(0, windowValue)
  				general.exp12 = c(general_left, general_right)
  				priors.exp12 = c(causal_prior, real_prior, unreal_prior)
  				
  				# generate webppl model with given parameters
  				exp1.fit.df = exp1.study.df %>% rowwise() %>% 
  				  mutate(modelPrediction = runModel(offset, signals, c(0,0,0,0,0), 1,
  				                                    noise.exp12, window.exp12, general.exp12,
  				                                    priors.exp12, c(spatialValue, velocity), samples))
  				exp2.fit.df = exp2.study.df %>% rowwise() %>% 
  				  mutate(modelPrediction = runModel(offset, signals, c(0,0,0,0,0), 1,
  				                                    noise.exp12, window.exp12, general.exp12,
  				                                    priors.exp12, c(spatialValue, velocity), samples))

  				# Model Fit Evaluation
  				# fit the data with bounds < 0 and 1
  				# Ensures data is in [eps, 1-eps]
  				x1 = epsilon + (1 - 2 * epsilon) * exp1.fit.df$modelPrediction
  				x2 = epsilon + (1 - 2 * epsilon) * exp2.fit.df$modelPrediction
  				
  				exp1_loglik = sum(exp1.fit.df$numOnes * log(x1) + exp1.fit.df$numZeros * log(1 - x1))
  				exp2_loglik = sum(exp2.fit.df$numOnes * log(x2) + exp2.fit.df$numZeros * log(1 - x2))
  				
  				# Metric on which to evaluate models
  				exp12_loglik = exp1_loglik + exp2_loglik
  				
  				# Data Saving
  				# make a new dataframe with necessary information
  				exp12.fit.df = rbind(exp1.fit.df %>% mutate(loglik = exp1_loglik), 
  				                     exp2.fit.df %>% mutate(loglik = exp2_loglik)) %>% 
  				  ungroup() %>% mutate(loglikTotal = exp12_loglik)

  				# save csv with essential information
  				# name is a combination of the values
  				title = c("exp12", "run", currentValue, 
  				          "n", visionValue, sensoryValue, "w", windowValue, 
  				          "p", real_prior, unreal_prior, "s", spatialValue)
  				write.csv(exp12.fit.df, paste("modelResults/",paste(title, collapse = "_"),".csv",sep=""), row.names=FALSE)
			    }
      }
		}
	}
}

endTime = Sys.time()
print(endTime - startTime)
