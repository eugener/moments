package org.oxbow.moments

import java.util.Date
import Moments._

final case class DateRange( val begin: Option[Date] = None, val end: Option[Date] = None ) {

    require( isOpen || begin.get <= end.get, "DateRange: Begin date should be <= end date"  )
    
    def includes( date: Option[Date] ): Boolean = date.exists( d => begin.forall( d >= _ ) && end.forall( d <= _ ))
    
    def includes( range: DateRange ): Boolean = includes( range.begin ) && includes( range.end )
    
    def intersects( range: DateRange ): Boolean = includes( range.begin ) || includes( range.end )
    
//    def intersection( range: DateRange ): Option[DateRange] = {
//        
//        if (!intersects( range )) return None
//        None
//    }
    
    def expand( beginAmount: TimeUnit=0.seconds, endAmount: TimeUnit=0.seconds  ): DateRange = {
        
        val a = if (isBeginOpen) None else Some( begin.get - beginAmount )
        val b = if (isEndOpen)   None else Some( end.get + endAmount )
        DateRange( a, b )
        
    }
    
    def isBeginOpen = begin.isEmpty
    
    def isEndOpen = end.isEmpty
    
    def isOpen = isBeginOpen || isEndOpen
    
    def isInfinite = isBeginOpen && isEndOpen
    
    def isEmpty = begin == end && !isInfinite
    
}