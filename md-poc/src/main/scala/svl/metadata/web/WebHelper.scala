//package svl.web
//
//import svl.mongo.AbstractFilter
//import javax.servlet.http.HttpServletRequest
//
//object WebHelper {
//  def addStringFilter(request:HttpServletRequest, filter: AbstractFilter, parName:String, fieldName:String) = {
//    val parValue = request.getParameter(parName)
//    if (parValue != null)
//      filter.filterEquals(fieldName, parValue)
//
//    filter
//  }
//
//  def addIntFilter(request:HttpServletRequest, filter: AbstractFilter, parName:String, fieldName:String) = {
//    val parValue = request.getParameter(parName)
//    if (parValue != null) {
//      if (parValue.startsWith(">"))
//        filter.filterGT(fieldName, Integer.parseInt(parValue.substring(1)))
//      else if (parValue.startsWith("<"))
//        filter.filterLT(fieldName, Integer.parseInt(parValue.substring(1)))
//      else
//        filter.filterEquals(fieldName, Integer.parseInt(parValue))
//    }
//
//    filter
//  }
//}
