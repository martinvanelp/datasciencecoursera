Model for word prediction
========================================================
author: Martin van Elp (@martinvanelp)
date: 24/1/2016

How the model predicts
========================================================

1. from a body of texts (news articles, blogs and tweets) a number of sentences have been sampled
2. these sentences have been split in sequences of four words and these sequences are stored (each word separately)
3. then this is compressed by counting the occurence of each sequence and storing that count for each unique sequence
4. this dictionary then consists of unique four word sequences that can be queried
5. the fourth word that occurs most often, given the preceeding three is the predicted outcome
6. if either of these three preceding words cannot be found, then they are systematically dropped to check less strictly for similar sequences


What results the model produces
========================================================

## Example predictions

- "A case of": 1. the; 2. beer; 3. a
- "A can of": 1. soup; 2. whoop; 3. worms 
- "I believe": 1. that; 2. in; 3. it
- No input:  1. i; 2. the; 3. it

## prediction <- function(dict, w1, w2, w3, n = 45)
* dict: which dictionary to use (customizable for each platform)
* w1, w2, w3: preceding three words (can be blank)
* n: The prediction function querying the dictionary can produce 45 predictions maximum, applying lesser strict rules when getting closer to that maximum.


How the app works
========================================================

![image of the app](app.png)

- you simply enter (part of) your sentence in the box to the left
- the first three predictions (ranked) are shown to the right interactively


Room for improvement
========================================================

- adding more corpora to the analysis side, to make predictions less reliant on the input used to create the dictionary
- optimizing the count for each unique set of four words and weighing predictions based on how strict the query was
- optimizing the size of the dictionary:
  - a function was made to replace the first three words of each sequence with three unique numbers but this did not decrease overall dictionary size
  - stemming could be an alternative here

