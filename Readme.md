
## HierDmap: Visualizing the geo-biodiversity profiles and the hierarchical structure of biodiversity


HierDmap is designed to visualize the biodiversity (especially for genetic diversity) in hierarchies and plot the geo-diversity distribution/density map. In the hierarchy plot, the circle (node) represents a aggregate at a certain hierarchical level and the circles inside of it (sub-nodes) represents sub-aggregate at a low level. The size of each aggregate is proportional to the diversity value.


### Install packages

Install the most recent version of the HierDmapC package using devtools:
`````{r}
library("devtools")

devtools::install_github("xinghuq/HierDmap")
``````
Alternatively, you can install from the source files, run the following commands in the shell:

```{shell}
R CMD build HierDmap
R CMD check --as-cran HierDmap_0.5.0.tar.gz
R CMD INSTALL HierDmap_0.5.0.tar.gz
```


### Dependencies

Before install or during installation, make sure the below dependences are installed.
``````{r}
if (!requireNamespace("devtools", quietly = TRUE))
    utils::install.packages("devtools")

if (!requireNamespace("circlepackeR", quietly = TRUE))
    devtools::install_github("jeromefroe/circlepackeR")
  
``````

### Vignettes and tutorials

``````{r}

vignette("Introduction")

``````



Welcome any [feedback](https://github.com/xinghuq/HierDmap/issues) and [pull request](https://github.com/xinghuq/HierDmap/pulls). 


### Citation

Qin. X. 2020. HierDmap: Visualizing the geo-biodiversity profiles and the hierarchical structure of biodiversity. R package version 0.5.0.

Qin, X. (2019). HierDpart: partitioning hierarchical diversity and differentiation across metrics and scales, from genes to ecosystems. R package version 0.5. 0 https://cran.r-project.org/package=HierDpart.
