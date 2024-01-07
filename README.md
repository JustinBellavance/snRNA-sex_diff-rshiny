## snRNA-seq Differential Expression Analysis Visualization

An interactive shiny app to compare male vs female expression of mice spinal cords, from snRNA-seq data. You can look at the volcano plot or the raw data matrix.

A volcano plot is a type of scatter plot represents differential expression of features (genes for example): on the x-axis we typically find the fold change and on the y-axis the p-value.

### ShinyApp can be found at https://justinbellavance.shinyapps.io/snRNA_Visualization/

### Application features

Once running the application opens to a tab with your volcano plot. Hover over the plot points to view geneID and other metrics.

On the left hand sidebar you'll find various ways to customize and annotate your plot including setting the axes variables, coloring the plot by differentially expressed gene, and labeling specific genes.

Easily download your volcano plot as a `.pdf` by clicking the download button.

![](/assets/volcanoPlotScreenShot.png)

On the second tab you'll find a rendered data table of the uploaded dataset that can be filtered to only show differentially expressed genes.

![](/assets/dataScreenShot.png)

### Forked from https://github.com/FredHutch/interactiveVolcano

The workhorse of this code and description was forked from https://github.com/FredHutch/interactiveVolcano.

The main optimization was I did to this software was using server-side selectize instead of client-side select, as well as adding specificity to be compabitible with the snRNA-seq mice spinal cord data.

I also changed features in line with potential users of the tool. (Such as tabs, and simplifying the options to reduce confusion)

### Big thank you to @lwolfe on Github and the Fred Hutch Cancer Center for making their code open source. 
