# enrich #

**enrich** is a R package used for gene enrichment analysis.

## Installation ##

This package can be installed from Github.

```
devtools::install_github("wolfsonliu/enrich")
```

## Usage ##

For easy use, the most usefull functions are `enrichKEGGPathway` and `enrichMSigDB`.

`enrichKEGGPathway` is used for pathway enrichment analysis. Input gene lists and pathway lists you care, and the function will output p value of enrichment. Make sure you have the internet connection, and function `enrichKEGGPathway` will automatically download KEGG pathway information from [KEGG website](http://www.kegg.jp/).

```
data("deg")

de.entrez <- as.character(na.omit(deg$entrezgene))

enrich.result <- enrichKEGGPathway(
    entrez.gene.list  = de.entrez,
    pathway.list      = NA,
    whole.gene.number = 20000,
    threshold         = 0.05,
    adjust.p.method   = "BH",
    organism          = "hsa"
)

enrich.result
```

`enrichMSigDB` is used for enrichment of gene sets in [MSigDB](http://software.broadinstitute.org/gsea/msigdb). Just like `enrichKEGGPathway`, input gene lists, and you will get the result. You can imput the msigdb data with appropriate data structure or use the data in the package. If you do not input msigdb data, the function will use the data from the package automatically. Gene sets in MSigDB include: hallmark gene sets, positional gene sets, curated gene sets, motif gene sets, computational gene sets, GO gene sets, oncogenic signatures, and immunologic signatures.

```
data("deg")

de.entrez <- as.character(na.omit(deg$entrezgene))

enrich.result <- enrichMSigDB(
    entrez.gene.list  = de.entrez,
    msigdb            = NA,
    whole.gene.number = 20000,
    threshold         = 0.05,
    adjust.p.method   = "BH"
)

enrich.result
```

There are also other functions, you can find the useage in R with `help` function.

### Information ###

* **Author**: Zhiheng Liu
* **Version**: 0.1
* **License**: LGPL-3

Please contact me if you find bug in the enrich R package. Thanks!
