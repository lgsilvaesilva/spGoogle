# DescriptionText <- function (text, parent = NULL, cdata = TRUE, sep = "") 
# {
    # node = newXMLNode("description", parent = parent)
    # text = paste(text, collapse = sep)
    # addChildren(node, if (cdata) 
        # newXMLCDataNode(text)
    # else text)
    # node
# }

DescriptionFig <- function(fig.path, height = 350, width = 350){
    desc.fig <- sprintf("<img src=\"%s\" height=\"%s\" width=\"%s\" >", fig.path, height, width)
	return(desc.fig)
}

DescriptionTable <- function (x, iframe = NULL, caption = "", fix.enc = TRUE, 
    cwidth = 150, twidth = 300, delim.sign = "_", asText = TRUE)
{
    x <- cbind(rownames(t(t(x))),t(t(x)))
    x[, 1] <- iconv(x[, 1], to = "UTF-8")

	
    if (!is.null(iframe)) {
        l1 <- newXMLNode("iframe", attrs = c(src = iframe, width = twidth * 
            2))
    }
    else {
        l1 <- newXMLNode("table", attrs = c(width = twidth, border = "0", 
            cellspacing = "5", cellpadding = "10"))
        l2 <- newXMLNode("caption", caption, parent = l1)
        txt <- sprintf("<tr><th width=\"%.0f\" scope=\"col\"><div align=\"right\"><strong>%s</strong>: </div></th><th scope=\"col\"><div align=\"left\">%s</div></th></tr>", 
            rep(cwidth, nrow(x)), paste(x[, 1]), paste(x[, 2]))
        parseXMLAndAdd(txt, l1)
    }
    if (asText == TRUE) {
        md.txt <- saveXML(l1)
        return(md.txt)
    }
    else {
        return(l1)
    }
}
