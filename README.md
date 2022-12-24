## snRNA-seq Differential Expression Analysis Visualization

An interactive shiny app to compare male vs female expression of mice spinal cords, from snRNA-seq data. You can look at the volcano plot or the raw data matrix.

A volcano plot is a type of scatter plot represents differential expression of features (genes for example): on the x-axis we typically find the fold change and on the y-axis the p-value.

### Forked from https://github.com/FredHutch/interactiveVolcano

The workhorse of this code and description was forked from https://github.com/FredHutch/interactiveVolcano. 

A couple optimizations (mainly using server-side selectize instead of client-side select) and specificity was added to be compabitible with the snRNA-seq mice spinal cord data.

### ShinyApp can be found at https://justinfuzz.shinyapps.io/snRNA_Visualization/?_ga=2.190652331.181211097.1671848990-1009044673.1671848990

### Application features

Once running the application opens to a tab with your volcano plot. Hover over the plot points to view geneID and other metrics.

On the left hand sidebar you'll find various ways to cuostmize and annotate your plot including setting the axes variables, coloring the plot by differentially expressed gene, and labeling specific genes.

Easily download your volcano plot as a `.pdf` by clicking the download button.

![](/assets/volcanoPlotScreenShot.png)

On the second tab you'll find a rendered data table of the uploaded dataset that can be filtered to only show differentially expressed genes.

![](/assets/dataScreenShot.png)

### Big thank you to @lwolfe on Github and the Fred Hutch cancer center for making their code open source. 
