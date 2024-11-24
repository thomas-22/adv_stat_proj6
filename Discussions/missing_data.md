### Update on hunting time imputation

#### If we would like to do imputation...

Whether kNN is viable depends on how accurate the predictions should be. If our analysis should be precise down to minute level, then kNN is not useful. See branch `impute_hunting`.

If our desired precision is hour level, it might work. In that case we might also consider not interpolating the movement data but only compute the distance between the hunting event and the temporally closest movement entry on the hour as in Ziqi's implementation.

#### It might be possible to circumvent the imputation issue.

Here is a new model proposal.

For each FCM sample, define 
- *time_recorded* = 1 if the last hunting event has recorded time; 0 if the last hunting event has missing time;
- *time_missing* = 1 whenever time_recorded = 0.

The last hunting event counts, only if it happens within 30 hours prior to defecation. (The threshold is still debatable.) If the last hunting event is, say, 3 months, old, we rather say that there is *no recent last hunting event*.

The linear mixed model contains the following covariates:

| Covariate | Explanation |
|---|---|
| hours since last hunting event * time_recorded | If we know the precise time, we use it. |
| distance to last hunting event * time_recorded | Same as above. |
| time_missing | If we do not know the precise time, we cannot be more precise. |
| number of hunting events within last 2 days, with a distance below 10 km | 2 and 10 are just assumptions. |
| average distance to last hunting event * time_missing | If we do not know the precise time, we cannot compute the distance. Taking the average is probably not the best idea. |
| last ng_g value of the same deer, if the last sample is not older than 30 hours; 0 otherwise. | We count 30 hours backwards from defecation time. Any sample in this time period is (assumed to be) correlated with the current sample. Older samples are (assumed to be) irrelevant. |
| interaction between last ng_g value of the same deer and time since last sample | The thersholding mechanism for the previous covariate also applies here. |
| Potential confounders | Grouping in winter enclosures, season, and potentially also pregnancy status.
| Time between defecation and sampling | There are three FCM samples without timestamp. So maybe we do the same as for hunting events.
| Random intercept for each deer | |

