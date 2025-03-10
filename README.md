# variation-time-series
A project exploring the role of variance and variation in time series data.

## Data
Data are downloaded from [PETS (v1.1)](https://pets.nhm.uio.no/).

For now, we are focusing on linear trait data, with at least 30 samples per time step and 6 time steps total.

We convert all data to log-space, using paleoTS for those that are not already transformed.

## Questions

1. Does variance (in log-space) stay constant even as trait means move away from an overall mean (or median or optimum)?
   * H0: slope of 0 between trait variance and absolute distance from mean, indicating that variance is the same.
   * H1: positive relationship, indicating an increase in variance as the trait moves away from the overall mean. This means that evolvability increases as traits move away from the overall mean. 
   * H2: negative relationship, indicating variance decreases as the trait moves away from the overall mean. This means that evolvability decreases as traits move away from the overall mean.

2. How do individual slopes compare to the overall pattern?
   * H0: majority (more than 50%?) of individual slopes overlap with the overall slope.
   * H1: individual slopes do not reflect the overall slope. If this is the case, then need to explore alternative explanations, such as role of sampling and error.

3. Does this pattern differ if the mean is increasing or decreasing relative to an overall mean?
   * H0: slopes show the same pattern (i.e., increase or decrease) as a trait moves away from the mean.
   * H1: as a trait gets larger, variance increases, and as a trait gets smaller, variance decreases.

4. Is the pattern different if the mode of evolution is directional or not?
   * H0: variances incease as traits move away from the mean for time series undergoing stasis (i.e., maintaining variation that includes the overall mean, maybe indicative of plasticity).
   * H1: variances do not change as traits move away from the mean for time series undergoing an UO process.
   * H2: no difference in pattern.
