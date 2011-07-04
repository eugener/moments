package org.oxbow.moments

import java.util.Date
import Moments._


final case class DateRange( val begin: Option[Date] = None, val end: Option[Date] = None ) {

    def this( a: Date, b: Date ) = this( Option(a), Option(b))
    def this( d: Date ) = this( d, d )
    
    require( isOpen || begin.get <= end.get, "DateRange: Begin date should be <= end date"  )
    
    def includes( date: Date ): Boolean = Option(date).exists( d => begin.forall( d >= _ ) && end.forall( d <= _ ))
    
    def includes( range: DateRange ): Boolean = includes( range.begin.get ) && includes( range.end.get )
    
    def intersects( range: DateRange ): Boolean = includes( range.begin.get ) || includes( range.end.get )
    
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
    
    def expand( amount: TimeUnit ): DateRange = expand( amount, amount )
    
    def shiftBack( amount: TimeUnit ): DateRange = expand( amount, -amount )
    def << ( amount: TimeUnit ): DateRange = shiftBack( amount )
    
    def shiftForward( amount: TimeUnit ): DateRange = expand( -amount, amount )
    def >> ( amount: TimeUnit ): DateRange = shiftForward( amount )
    
    def isBeginOpen = begin.isEmpty
    
    def isEndOpen = end.isEmpty
    
    def isOpen = isBeginOpen || isEndOpen
    
    def isInfinite = isBeginOpen && isEndOpen
    
    def isEmpty = begin == end && !isInfinite
    
}