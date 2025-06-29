# Seeing, Hearing, and Feeling Causation

*Elyse D. Z. Chase (Rice University), Kevin Smith (Massachusetts Institute of Technology), Sean Follmer (Stanford University), Tobias Gerstenberg (Stanford University)*
*Submitted to Cognitive Science*

## Contents
- [Seeing, Hearing, and Feeling Causation](#seeing-hearing-and-feeling-causation)
  - [Contents](#contents)
  - [Overview](#overview)
  - [Repository structure](#repository-structure)
  - [Usage](#usage)
    - [Experimental Analysis](#experimental-analysis)
    - [Modeling](#modeling)
  - [CRediT author statement](#credit-author-statement)

## Overview

How do people decide whether one event caused another to happen? Prior work has examined how people use visual and auditory cues in causal perception. Here, we explore the role of touch. We develop a multi-modal inference model that uses visual, auditory, and haptic information to determine causation. The model is sensitive to the relative timing of the sensory cues and the uncertainty associated with it. We test the model in three psychophysical studies. In Experiment 1, we added force-based haptic feedback to a visual launching paradigm. We found that the additional haptic information increased the likelihood of a causal response compared to vision alone. In Experiment 2, we tested all combinations of vision, audio, force-based haptics, and vibrotactile haptics. We found that adding any sensory information increased the likelihood of a causal response beyond vision alone but that these increases did not continue with additional sensory signals – there are diminishing returns. In Experiment 3, we explored the role of multisensory information and physical realism in people’s causal perception. Here, the number of multisensory cues and their physical realism increased causal judgments. Our findings will help advance our understanding of how humans integrate multisensory information to infer causality and inform applications in virtual reality, robotics, and human–computer interaction. Future research could work with more complex, real-time, and dynamic environments and investigate individual differences in prior experiences with multisensory interaction.

**Keywords**: multisensory; haptics; kinesthetic; vibrotactile; audition; inference model; causality

## Preregistrations

Preregistrations for all experiments are available on the Open Science Framework (OSF):
- [Experiment 1](https://doi.org/10.17605/OSF.IO/58UN9)
- [Experiment 2](https://doi.org/10.17605/OSF.IO/JMZ8Q)
- [Experiment 3](https://doi.org/10.17605/OSF.IO/CYD8J)

## Repository structure

```
├── analysis
│   ├── experimental
│   │   ├── analysis_exp1_kinesthetic.Rmd
│   │   ├── analysis_exp2_multisensory.Rmd
│   │   └── analysis_exp3_realism.Rmd
│   ├── model
│   │   ├── finalFit.Rmd
│   │   ├── relatedWork_finalFit.Rmd
│   │   └── results
├── modeling
│   ├── fitting_exp12/exp3/guski/meding/wang.R
│   ├── webppl
│   ├── humanData
│   ├── relatedWork
├── data
│   ├── exp1_kinesthetic
│   ├── exp2_multisensory
│   ├── exp3_realism
│   │   ├── a
│   │   └── b
│   └── model
│       ├── relatedWork
│       └── perceptualData
```

## Usage

### Experimental Analysis

All analysis is performed via RMarkdown. To regenerate the results from the analysis on our modeling, run:

```r
# In the R console or an R script
document_files <- list(
  "analysis/experimental/analysis_exp1_kinesthetic.Rmd",
  "analysis/experimental/analysis_exp2_multisensory.Rmd",
  "analysis/experimental/analysis_exp3_realism.Rmd"
)

lapply(document_files, rmarkdown::render)
```

### Modeling

The base R files in `modeling/` will generate CSV files with the error of the model compared to human data. They expect to be given the model parameters as arguments. *For the results in the paper, these files were run on a shared high-performance cluster*.

The best-performing fits can be plotted using `modeling/finalFit.Rmd` on the results of the base R files, and our best fits are in `modeling/results/`.


## CRediT author statement

- **Elyse D.Z. Chase**: Conceptualization, Methodology, Software, Validation, Formal analysis, Investigation, Resources, Data Curation, Writing - Original Draft, Writing - Review & Editing, Visualization
- **Kevin Smith**: Conceptualization, Methodology, Formal analysis, Writing - Review & Editing, Supervision
- **Sean Follmer**: Conceptualization, Methodology, Writing - Review & Editing, Supervision, Project administration, Funding acquisition
- **Tobias Gerstenberg**: Conceptualization, Methodology, Validation, Formal analysis, Writing - Review & Editing, Supervision, Project administration, Funding acquisition
