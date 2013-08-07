## vet      - Vetor a ser quebrado.
## cuts     - Número de quebras do vetor ou vetor com as quebras.
## cut.type - Tipo de quebra(range, quantile).

CutsVector <- function(vet, cuts, cuts.type = NULL){

  if (length(cuts) == 1 & is.null(cuts.type)){
    stop('cuts.type = \'range\' or \'quantile\'')
  }

  if (length(cuts) != 1 & is.numeric(cuts)) {
    brks <- cuts
    num.faixas <- length(brks) - 1
    cuts.type <- FALSE
  }else{
     num.faixas <- cuts
  }

  if (!is.null(cuts.type)){
    if (cuts.type == "range") {
      maxi <- max(vet, na.rm = TRUE)
      mini <- min(vet, na.rm = TRUE)
      ampl <- diff(range(vet, na.rm = TRUE))	
      faixa.calc <- seq(mini, maxi, by = (ampl)/cuts)	
      brks  <- faixa.calc
    }

    if (cuts.type == "quantile") {
      maxi <- max(vet, na.rm = TRUE)
      mini <- min(vet, na.rm = TRUE)
      prob <-  seq(1/cuts, by = 1/cuts)
      faixa.calc <- quantile(vet, prob = prob[-length(prob)], na.rm = TRUE)
      brks <- c(mini, faixa.calc, maxi)
    }
  }
  output <- list(brks = brks, num.faixas = num.faixas)
  return(output)
}

