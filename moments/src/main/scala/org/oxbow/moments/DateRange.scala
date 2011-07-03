package org.oxbow.moments

import java.util.Date
import Dates._

final case class DateRange( val begin: Option[Date] = None, val end: Option[Date] = None ) {

    require( isOpen || begin.get <= end.get, "DateRange: Begin date should be <= end date"  )
    
    def includes( date: Option[Date] ) : Boolean = date match {
        case None    => false
        case Some(d) => (begin.isEmpty || d >= begin.get ) && ( end.isEmpty || d <= end.get )  
    }
    
    def includes( range: DateRange ): Boolean = includes( range.begin ) && includes( range.end )
    
    def intersects( range: DateRange ): Boolean = includes( range.begin ) || includes( range.end )
    
    def isBeginOpen = begin.isEmpty
    
    def isEndOpen = end.isEmpty
    
    def isOpen = isBeginOpen || isEndOpen
    
    def isInfinite = isBeginOpen && isEndOpen
    
    def isEmpty = !isInfinite && begin == end
    
}