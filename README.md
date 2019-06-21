# tick_sampling
Code and data for tick drag sampling manuscript

Drag cloth sampling is a standard method to sample for questing ticks. A drag cloth is dragged along the ground/over vegetation and checked for ticks to regular intervals. The distance between these intervals varies across studies, from as little as 5 m to as much as 30 m. If attached ticks drop off the drag cloth as it is dragged checking the drag cloth at greater distance intervals could result in a lower tick density estimate. Here we explicitly test the rate at which adult and nymphal _Ixodes scapularis_ ticks drop off the drag cloth per meter dragged. We use this rate to calculate the reduction estimate of tick density if the drag cloth is checked every 10 m, 20 m or 30 m. We then then test these predictions by conducting this variable check distance experiment.

`dropoff_rate_data.csv` has the data from the experiment were we estimate the rate at which ticks drop off the drag cloth.

`variable_drag_dist_data.csv` had the data from the experiment were we measure tick density with drag sampling checking at difference distance intervals.

`r_code.R` has the code to analyze these experiments.

This analysis is part of a manuscript in preparation.
