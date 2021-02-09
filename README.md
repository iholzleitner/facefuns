
<!-- README.md is generated from README.Rmd. Please edit that file -->

# facefuns

<!-- badges: start -->

<!-- badges: end -->

Miscellaneous convenience functions around geometric morphometric
analyses of 2D and 3D face data. A brief intro can be found
[here](https://iholzleitner.github.io/facefuns/articles/facefuns.html)\!

Last update: 2021-02-09

## Installation

You can install the development version of `facefuns` from
[GitHub](https://github.com) with:

``` r
# install.packages("devtools")
devtools::install_github("iholzleitner/facefuns")
```

If you get a 404 error, try this instead:

``` r
devtools::install_github("iholzleitner/facefuns", ref = "main")
```

## Credits

Many of the functions in this package are tweaks on existing functions
written by others, and for some functions code has been adopted
verbatim.

  - **`geomorph`**: Many functions in this package either use `geomorph`
    functions, or use code adapted from `geomorph`, which is licensed
    under GPL-3.  
    Adams, D. C., Collyer, M. L. & Kaliontzopoulou, A. (2019) Geomorph:
    Software for geometric morphometric analyses. R package version
    3.1.0. <https://cran.r-project.org/package=geomorph>

  - **Select PCs using Broken Stick criterion**: adapted from `evplot`
    by Francois Gillet
    <http://adn.biol.umontreal.ca/~numericalecology/numecolR/> (from
    material for first edition, GPL-2)

  - **Re-create 3D mesh from point cloud**: adapted from
    `nat::as.mesh3d.ashape3d`  
    Jefferis, G. S. X. E. & Manton, J. D. (2014). NeuroAnatomy Toolbox
    v1.5.2. ZENODO. <https://doi.org/10.5281/zenodo.10171>

  - **Flat violin plots**: by Ben Marwick  
    <https://gist.githubusercontent.com/benmarwick/2a1bb0133ff568cbe28d/>

  - **Split violin plots**: by StackOverflow user jan-glx  
    <https://stackoverflow.com/questions/35717353/split-violin-plot-with-ggplot/>
