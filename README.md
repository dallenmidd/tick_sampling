# tick_sampling
Code and data for the paper: B. Borgmann-Winter and D. Allen. 2020. How the distance between drag-cloth checks affects the estimate of adult and nymphal _Ixodes scapularis_ (Acari: Ixodidae) density. _Journal of Medical Entomology_ 57: 623-626. https://doi.org/10.1093/jme/tjz179. This paper is available to all readers on [PubMed](https://www.ncbi.nlm.nih.gov/pmc/articles/PMC7317148/). If you use these data or code please cite this paper, thanks.

Drag cloth sampling is a standard method to sample for questing ticks. A drag cloth is dragged along the ground/over vegetation and checked for ticks at regular intervals. The distance between these intervals varies across studies, from as little as 5 m to as much as 30 m. If attached ticks drop off the drag cloth as it is dragged, checking the drag cloth at greater distance intervals could result in a lower tick density estimate. Here we test for this effect by conducting two experiments. In the first, we measure the rate at which adult and nymphal _Ixodes scapularis_ ticks drop off the drag cloth per meter dragged. We use this rate to predict the reduction in estimated tick density if the drag cloth is checked every 10 m, 20 m or 30 m. In the second experiment, we then check these predicted reductions by conducting this variable check distance experiment. 

`dropoff_rate_data.csv` has the data from the experiment where we estimate the rate at which ticks drop off the drag cloth.

`variable_drag_dist_data.csv` had the data from the experiment where we measure tick density with drag sampling checking at different distance intervals.

`r_code.R` has the code to analyze these experiments.
