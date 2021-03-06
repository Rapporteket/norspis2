% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/NorSpis1_3_Utvalg.R
\name{NorSpis1_3_Utvalg}
\alias{NorSpis1_3_Utvalg}
\title{Funksjon som gjør utvalg av dataene, returnerer det filtrerte datasettet og utvalgsteksten.}
\usage{
NorSpis1_3_Utvalg(
  RegData,
  fargepalett = "BlaaOff",
  aar = 0,
  datoFra,
  datoTil,
  datoFraSluttreg = "",
  datoTilSluttreg = "",
  minald = 0,
  maxald = 130,
  minbmistart = 0,
  maxbmistart = 99999,
  regType = "",
  enhetstypeDogn = "",
  enhetstypeRegional = "",
  erMann = "",
  enhetsUtvalg = 0,
  reshID = 0,
  diagnose = ""
)
}
\arguments{
\item{diagnose}{}
}
\value{
UtData En liste bestående av det filtrerte datasettet (RegData), utvalgstekst for
figur (UtvalgTxt), fargepalett, indekser for hovedgruppe og sammenlikningsgruppe (ind),
tekststreng som angir fargepalett,  om vi har sammenlikning eller ikke (medSml), tekst som angir
hovedgruppa (hovedgrTxt) og gruppa det evt. sammenliknes mot (smltxt)
}
\description{
Funksjon som gjør utvalg av dataene, returnerer det filtrerte datasettet og utvalgsteksten.
}
