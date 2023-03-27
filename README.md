# Outline
The data processing in this repo has mostly been done in R, with training of ABSA models done in python. The respective source files can be found in the `code` folder. They are numbered for the order in which they have been run. The code folder also contains the annotated data set created for this thesis, as `pyABSA` has a strange way of handling paths, so input and output files of `code/04-train.ipynb` were just left in that directory, except for the trained models, which are in `mv_checkpoints`.

The file `python-env-personalities.yml` contains the installed packages in the conda environment used for this project.  
Consult the session info printout below for version of used packages:
``` r
sessionInfo()
#> R version 4.2.3 (2023-03-15)
#> Platform: x86_64-redhat-linux-gnu (64-bit)
#> Running under: Fedora Linux 37 (Workstation Edition)
#> 
#> Matrix products: default
#> BLAS/LAPACK: /usr/lib64/libflexiblas.so.3.3
#> 
#> locale:
#>  [1] LC_CTYPE=en_US.UTF-8       LC_NUMERIC=C              
#>  [3] LC_TIME=en_US.UTF-8        LC_COLLATE=en_US.UTF-8    
#>  [5] LC_MONETARY=en_US.UTF-8    LC_MESSAGES=en_US.UTF-8   
#>  [7] LC_PAPER=en_US.UTF-8       LC_NAME=C                 
#>  [9] LC_ADDRESS=C               LC_TELEPHONE=C            
#> [11] LC_MEASUREMENT=en_US.UTF-8 LC_IDENTIFICATION=C       
#> 
#> attached base packages:
#> [1] stats     graphics  grDevices utils     datasets  methods   base     
#> 
#> loaded via a namespace (and not attached):
#>  [1] digest_0.6.31     withr_2.5.0       R.methodsS3_1.8.2 lifecycle_1.0.3  
#>  [5] magrittr_2.0.3    reprex_2.0.2      evaluate_0.20     rlang_1.1.0      
#>  [9] cli_3.6.0         rstudioapi_0.14   fs_1.6.1          R.utils_2.12.2   
#> [13] R.oo_1.25.0       vctrs_0.6.0       styler_1.9.1      rmarkdown_2.20   
#> [17] tools_4.2.3       R.cache_0.16.0    glue_1.6.2        purrr_1.0.1      
#> [21] xfun_0.37         yaml_2.3.7        fastmap_1.1.1     compiler_4.2.3   
#> [25] htmltools_0.5.4   knitr_1.42
```