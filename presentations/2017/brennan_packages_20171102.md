
How test() and check() showed my bad code
========================================================
author: Dr Paul Brennan

![logo](https://github.com/brennanpincardiff/RforBiochemists/raw/master/logo.png)

2 November 2017

[R for Biochemists] (http://rforbiochemists.blogspot.co.uk)

drawProteins
========================================================
My aim was to create a package that would allow visualisation of proteins given data from [Uniprot website](http://www.uniprot.org/).

Workflow:
- Get data using [UniProt API](http://www.uniprot.org/help/programmatic_access)
- Turn data from JSON into dataframe
- Visualise using ggplot2



drawProteins - demo - step 1
========================================================

```r
library(magrittr)
library(drawProteins)
library(httr)
library(ggplot2)

# accession numbers of hair keratin
"Q14533 P19012 P02538" %>%
  drawProteins::get_features() ->
  protein_json
```

```
[1] "Download has worked"
```

drawProteins - demo - step 2
========================================================

```r
# turn JSON object into a dataframe
protein_json %>%
  drawProteins::feature_to_dataframe() ->
  prot_data
```

drawProteins - demo - step 3
========================================================

```r
# series of functions to visualise
prot_data %>%
  geom_chains() %>%
  geom_domains() %>%
  geom_region %>%
  geom_motif %>%
  geom_phospho(size = 8) -> p

p <- p + theme_bw(base_size = 20) +  # white background and change text size
  theme(panel.grid.minor=element_blank(),
        panel.grid.major=element_blank()) +
  theme(axis.ticks = element_blank(),
        axis.text.y = element_blank()) +
  theme(panel.border = element_blank())
```

drawProteins - output
========================================================
<img src="brennan_packages_20171102-figure/code_demo_4-1.png" title="plot of chunk code_demo_4" alt="plot of chunk code_demo_4" width="1000px" height="600px" />

drawProteins - works - let's make a package
========================================================
**New concepts**
- Building my package - [Travis CI](https://travis-ci.org/brennanpincardiff/drawProteins)
- `devtools::document()` - [Roxygen tags](http://r-pkgs.had.co.nz/man.html#man-functions)
- [`devtools::test()`](http://r-pkgs.had.co.nz/tests.html)
- [`devtools::check()`](http://r-pkgs.had.co.nz/check.html)




First Build - unsuccessful (of course)
========================================================
Finally with a bit of effort, managed to bring devtools::check() and [Travis CI](https://travis-ci.org/brennanpincardiff/drawProteins/builds) into parallel

No ERRORS but WARNINGS and NOTES
- missing documentation entries ... WARNING
- Extensions’ manual.checking installed package size ... NOTE



Learning curve
========================================================
**How to be a good documentor**
- Go back to [Hadley's book](http://r-pkgs.had.co.nz/tests.html) - learn about **Roxygen tags**

**Becoming a better tester**
- Write some [unit tests](https://github.com/brennanpincardiff/drawProteins/blob/master/tests/testthat/test-geoms.R)
- [Find out about layers in ggplot2](https://stackoverflow.com/questions/13457562/how-to-determine-the-geom-type-of-each-layer-of-a-ggplot2-object/43982598#43982598)



drawProteins - Bioconductor
========================================================

[Bioconductor](https://bioconductor.org/) has more checks than CRAN
- I want drawProteins to work with other Bioconductor packages
- Style guide for Vignette (add `bioc_required: true` to [.travis.yml](https://github.com/brennanpincardiff/drawProteins/blob/master/.travis.yml))
- [BiocCheck()](https://bioconductor.org/packages/release/bioc/html/BiocCheck.html) package
- ERROR: At least 80% of man pages documenting exported objects must have runnable examples.


Notes
========================================================

`devtools::check()` shows: 0 errors | 0 warnings | 1 note

- I can't ignore this any more
- This is really what showed my __bad code__
- `geom_domains: no visible binding for global variable ‘prot_data’`
- I didn't really understand this warning...
- I'd ignored it... just a NOTE....



Global variables... geom_chains...
========================================================

```r
# show the function
geom_chains <- function(prot_data = prot_data,
                        outline = "black",
                        fill = "grey",
                        label_chains = TRUE,
                        labels = prot_data[prot_data$type == "CHAIN",]$entryName,
                        size = 0.5,
                        label_size = 4){

    p <-ggplot2::ggplot() +
    ggplot2::ylim(0.5, max(prot_data$order)+0.5) +
```

Passing some arguments in e.g. prot_data BUT not all of them...


Global variables... geom_domains
========================================================

```r
geom_domains <- function(p,
                         label_domains = TRUE,
                         label_size = 4){
  p <- p +
    ggplot2::geom_rect(data= prot_data[prot_data$type == "DOMAIN",],
            mapping=ggplot2::aes(xmin=begin,
```
Clearer here: `prot_data` and `begin` are __NOT__ passed in to the function so R has to find it...



This is bad code....ARGHH!!!
========================================================
At the [CaRdiff User Group](https://www.meetup.com/Cardiff-R-User-Group/?scroll=true)
- we discussed variable scope
- but until I really addressed this __NOTE__
- I didn't really understand it!!!


I think I better think it out again!!!
========================================================
Options
- Pass in each of the variables...
- Learn how to allow some sort of inheriting of variables
- Make a proper ggplot2 extension
- Go to the pub and forget about coding completely!!!


Acknowledgements
========================================================
 - [Cardiff University](http://www.cardiff.ac.uk/) pays my salary
 - CaRdiff R-Users Group - [Stef Locke](https://itsalocke.com/) and [Dave Parr](https://github.com/DaveRGP)
 - [Biochemical Society](https://www.biochemistry.org/default.aspx) for support & advertising for the R for Biochemists Training day
 - Biochemcial Society - R for Biochemists 101
 - This presentation made as an [R Studio Presentation](http://rmarkdown.rstudio.com/)

Contacts
========================================================
 - Email: brennanp@cardiff.ac.uk
 - Twitter: @brennanpcardiff
 - [Github](https://github.com/brennanpincardiff)
 - [R for Biochemists] (http://rforbiochemists.blogspot.co.uk)






