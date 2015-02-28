zeit-2
======

## news analysis:
- access newspaper articles of Die Zeit
- clean them of html
- create a document term matrix
- evaluate sentiment using sentiws (publically available data base)
- identify topics 
- create sentiment indices for each topic over time.

## pseudo-out-of sample experiment:

- realtime_experiment.R collects the data and implents the experiment.

### changing the realtime experiment
I take the approach of bubblesbreakdowns:
- lag length selection is implemented based on bma methods (raftery et al.). 
- in contrast to the original bubblesbreakdowns approach, this will only be implemented for the last vintage.
- starting with the last vintage, the lag length estimated there will be used for all other vintages.

the steps to implent this:
1. step (getting process)
- the file realtime experiment.Rmd will be used including the data.
- it will be named "realtime experiment zeit.Rmd" 
- The sequence will be reversed, such that the loop starts at the latest vintage.
- one modified version of bmafo will be used that returns the last vintages lag length and the forecast.
- starting with the penultimate vintage, another modified version will use the lag length of the latest vintage and returns the forecast based on model estimated on the penultimate data.
2. step (getting data)
- bundesbank realtime data will be transformed to sets (containing one vintage of several variables)
- the unrevised data are read in.
- the revised and the unrevised data are plugged into the existing process from bubblesbreakdowns.

## goldstandard testing

- optimize zeit-index.Rmd contains the sentiment analysis used and compares an index created on its basis to a goldstandard.
