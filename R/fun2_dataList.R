#' List all the prepared datasets for norspis2 package
#'
#' This function runs the functions fun2_1_1.. to fun2_1_5 in the correct order, to make a list of
#' five datasets (the "2" suffix refers to that these data can be used to make figures and tables in the norspis2 package):
#'
#' * RegData2
#' * RegDataBeh2
#' * RegDataNatVal2
#' * RegDataStartEnd2
#' * RegDataStartEndNatVal2
#'
#' @param myInData1 RegData
#' @param myInData2 RegDataBeh
#'
#' @return dataList, a list with all five datasets
#' @export
#'
#' @examples

fun2_dataList <- function (myInData1=RegData, myInData2 = RegDataBeh){

  RegData <- myInData1
  RegDataBeh <- myInData2

  RegData <- norspis2::fun2_1_1_RegData_newVarGlobal(myInData=RegData)

  #2:
  RegDataBeh <- norspis2::fun2_2_RegDataBeh_newVarGlobal(myInData1=RegData, myInData2=RegDataBeh)
  #END dataset 2:RegDataBeh2

  #1:
  RegData <- norspis2::fun2_1_2_RegData_newVarFrmt(myInData=RegData)
  RegData <- norspis2::fun2_1_3_RegData_newVarMAsNA(myInData = RegData)
  RegData <- norspis2::fun2_1_4_RegData_newVarDich(myInData=RegData)
  #END dataset 1: RegData2

  #3:
  RegDataNatVal <- norspis2::fun2_3_RegDataNatVal(myInData=RegData)
  #END dataset 3: RegDataNatVal2

  #4:
  RegDataStartEnd <- norspis2::fun2_4_1_RegDataStartEnd(RegData)
  RegDataStartEnd <- norspis2::fun2_4_2_RegDataStartEnd_newVar(RegDataStartEnd)
  #END dataset 4: RegDataStartEnd2

  #5:
  RegDataStartEndNatVal <- norspis2::fun2_5_RegDataStartEndNatVal(RegDataStartEnd)
  #END dataset 5: RegDataStartEndNatVal2

  dataList <- list(RegData2 = RegData,
                   RegDataBeh2 = RegDataBeh,
                   RegDataNatVal2 = RegDataNatVal,
                   RegDataStartEnd2 = RegDataStartEnd,
                   RegDataStartEndNatVal2 = RegDataStartEndNatVal)
}
