### get all KEGG orthology
###
retrieve_kegg_orthology <- function(){
    kk <- keggList("ko")
    result <- c()
    for(k in names(kk)){
        ko <- unlist(strsplit(k,":"))[2]
        result <- c(result, ko)
    }
    return(result)
}

species_orthology_association <- function(sp, ko){
    matrix <- data.frame(matrix(nrow = length(sp), ncol = length(ko)))
    rownames(matrix) <- sp
    colnames(matrix) <- ko
    for(kk in ko){
        genes_set <- keggGet(kk)[[1]]$GENES
        for(i in 1:length(genes_set)){
            gene <- genes_set[i]
            if(!is.null(gene) & length(gene)>0){
                ul <- unlist(strsplit(gene,":"))
                row_indx <- which(rownames(matrix) == tolower(ul[1]))
                if(length(row_indx) >0){
                    col_indx <- match(k, colnames(m_matrix))
                    m_matrix[row_indx, col_indx] <- 1
                }
            }
        }
    }
}

get_diseases <- function(){
    disease <- read_excel("~/git-code/R/microbe_disease_association/data/disease.xlsx")
    return (disease)
}

get_microbes <-  function(){
    microbe <- read_excel("~/git-code/R/microbe_disease_association/data/microbe.xlsx")
    return (microbe)
}

get_microbe_disease <- function(){
    microbe_disease <- read_excel("~/git-code/R/microbe_disease_association/data/microbe-disease.xlsx")
    return (microbe_disease)
}

get_gaussian_disease <- function(){
    gaussian_disease <- read_excel("~/git-code/R/microbe_disease_association/data/Gaussian_disease.xlsx")
    return (gaussian_disease)
}

get_gaussian_microbe <- function(){
    gaussian_microbe <- read_excel("~/git-code/R/microbe_disease_association/data/Gaussian_microbe.xlsx")
    return (gaussian_microbe)
}

get_kegg_orgcode <- function(){
    dest_file <- "~/git-code/R/microbe_disease_association/generated/organism"
    if(!file.exists(dest_file)){
        url <- "http://www.kegg.jp/kegg-bin/show_organism?category=Bacteria"
        doc <- htmlParse(url)
        tableNodes <- getNodeSet(doc, "//table")
        org_code <- readHTMLTable(tableNodes[[4]], header = FALSE)
        if(is.data.frame(org_code) && nrow(org_code) !=0){
            write.table(org_code, file = dest_file, sep = "\t", row.names = FALSE, col.names = FALSE)
            return (org_code)
        }
    }
    data.frame  <- read.table(dest_file, sep = "\t")
    return (data.frame)
}