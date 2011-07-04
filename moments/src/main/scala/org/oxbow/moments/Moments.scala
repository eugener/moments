package org.oxbow.moments

import java.util.Date
import java.util.Calendar
import java.text.SimpleDateFormat
import java.util.TimeZone

object Moments {

    implicit def date2x( date: Date ) = new SuperDate( date )
    
    implicit def int2timeUnit( amount: Int ): TimeUnitInt = new TimeUnitInt( amount )
    
    
    final case class SuperDate( date: Date ) extends Ordered[Date] {
        
        /**
         * Returns a copy of the date with time unit added
         */
    	def +( unit: TimeUnit ): Date = unit.addTo( date )
    	
    	/**
    	 * Returns a copy of the date with time unit subtracted
    	 */
    	def -( unit: TimeUnit ): Date = (-unit).addTo( date )
    	
    	override def compare(that: Date): Int = {
    	    
    	    that match {
    	        case null => return -1
    	        case x if ( date==x ) => 0
    	        case x  => if ( date.before(x)) -1 else 1 
    	    }

    	}
    	
    	def calendar: Calendar = {
    	   val c = Calendar.getInstance
    	   c.setTime( date )
    	   c 
    	}
    	
    	/**
    	 * Returns a copy of the date with provided fields set to zero
    	 */
    	def clear( fields: Int* ): Date = {
    	    val c: Calendar = calendar
    	    fields.foreach( c.set( _, 0))
    	    c.getTime
    	}

    	/**
    	 * Returns a copy of the date with time portion set to zero
    	 */
    	def midnight: Date = clear( Hour, Minute, Second, Millisecond )
    	
    	/**
    	 * Returns a copy of the date representing first day of the month
    	 */
    	def monthBegin: Date = {
    	    val c = calendar
    	    c.set( Day, 1 )
    	    c.getTime.midnight
    	}
    	
    	/**
    	 * Returns a copy of the date representing last day of the month
    	 */
    	def monthEnd: Date = date.monthBegin + 1.month - 1.day
    	
    	
    	def era    = calendar.get(Era)
    	def year   = calendar.get(Year)
    	def month  = calendar.get(Month)
    	def day    = calendar.get(Day)
    	def hour   = calendar.get(Hour)
    	def minute = calendar.get(Minute)
    	def second = calendar.get(Second)
    	
    	def dayOfYear   = calendar.get(Calendar.DAY_OF_YEAR)
    	def dayOfWeek   = calendar.get(Calendar.DAY_OF_WEEK)
    	
    	def weekOfYear  = calendar.get(Calendar.WEEK_OF_YEAR)
    	def weekOfMonth = calendar.get(Calendar.WEEK_OF_MONTH)

    	/**
    	 * Formats date according to a format @see DateFormat
    	 */
    	def format( format: String ): String = (new SimpleDateFormat(format)).format(date) 
    	
    	/**
    	 * Returns GMT date 
    	 */
    	def gmt: Date = {
    	    
    	    val tz = TimeZone.getDefault
    	    val dt = new Date( date.getTime - tz.getRawOffset )
    	    
    	    // back off by delta if we're in DST
    	    if ( tz.inDaylightTime(dt) ) {
    	        
    	        var dstDate = new Date( dt.getTime - tz.getDSTSavings )
    	        
    	        // check if date is not crossed into standard time
    	        if ( tz.inDaylightTime(dstDate) ) return dstDate
    	    }
    	    
    	    dt
    	}
    	
    	def combine( end: Date ) = DateRange( Option(date), Option(end) )

    }
    
    
    /**
     * Abstract time unit
     */
    sealed trait TimeUnit {
        val amount: Int
        def addTo(date: Date): Date
        def setTo(date: Date): Date
        def unary_-(): TimeUnit
    }
    
    
    /**
     * Private implementation of time unit
     */
    private[this] case class TimeUnitImpl( private val field: Int, override val amount: Int) extends TimeUnit {
        
        override def addTo(date: Date ): Date = {
            val c: Calendar = date.calendar
            c.add( field, amount )
            c.getTime
        }
        
        override def setTo(date: Date): Date = {
            val c: Calendar = date.calendar
            c.set( field, amount )
            c.getTime
        }
        
        override def unary_-(): TimeUnit = copy( amount = -amount )
        
    }
    
    final class TimeUnitInt( amount: Int ) {

        lazy val eras: TimeUnit = TimeUnitImpl(Era, amount)
        lazy val era : TimeUnit = eras
        
        lazy val years: TimeUnit = TimeUnitImpl(Year, amount)
        lazy val year : TimeUnit = years
        
        lazy val months: TimeUnit = TimeUnitImpl(Month, amount)
        lazy val month : TimeUnit = months

        lazy val weeks: TimeUnit = TimeUnitImpl(Day, amount*7 )
        lazy val week : TimeUnit = months
        
        lazy val days: TimeUnit = TimeUnitImpl(Day, amount)
        lazy val day : TimeUnit = days
        
        lazy val hours: TimeUnit = TimeUnitImpl(Hour, amount)
        lazy val hour: TimeUnit  = minutes
        
        lazy val minutes: TimeUnit = TimeUnitImpl(Minute, amount)
        lazy val minute: TimeUnit  = minutes
        
        lazy val seconds: TimeUnit = TimeUnitImpl(Second, amount)
        lazy val second: TimeUnit  = seconds
        
        lazy val milliseconds: TimeUnit = TimeUnitImpl(Millisecond, amount)
        lazy val millisecond: TimeUnit  = milliseconds
        
    }
    
//    /** 
//     * Creates a date. 
//     * Era, year, month and day default to current date values 
//     * Hour, minute, second and millisecond default to zero 
//     */
//    def date( era: TimeField = -1, 
//              year: TimeField = -1, 
//              month: TimeField = -1,
//              day: TimeField = -1, 
//              hour: TimeField = 0,
//              minute: TimeField = 0,
//              second: TimeField = 0, 
//              millisecond: TimeField = 0 ): Date = {
//        
//        val c: Calendar = now.calendar
//        
//        if ( era >= 0 ) c.set( Calendar.ERA, era )
//        if ( year >= 0 ) c.set( Calendar.YEAR, year )
//        if ( month >= 0 ) c.set( Calendar.MONTH, month )
//        if ( day >= 0 ) c.set( Calendar.DAY_OF_MONTH, day )
//        c.set( Calendar.HOUR, hour )
//        c.set( Calendar.MINUTE, minute )
//        c.set( Calendar.SECOND, second )
//        c.set( Calendar.MILLISECOND, millisecond )
//        
//        c.getTime
//    }
    
    def date( units: TimeUnit* ): Date = {
        var d = now
        units.foreach( unit => d = unit.setTo(d) )
        d
    }
    
    final val Era         = Calendar.ERA
    final val Year        = Calendar.YEAR
    final val Month       = Calendar.MONTH
    final val Day         = Calendar.DAY_OF_MONTH
    final val Hour        = Calendar.HOUR
    final val Minute      = Calendar.MINUTE
    final val Second      = Calendar.SECOND
    final val Millisecond = Calendar.MILLISECOND
    
    final val January   = Calendar.JANUARY
    final val February  = Calendar.FEBRUARY
    final val March     = Calendar.MARCH
    final val April     = Calendar.APRIL
    final val May       = Calendar.MAY
    final val June      = Calendar.JUNE
    final val July      = Calendar.JULY
    final val August    = Calendar.AUGUST
    final val September = Calendar.SEPTEMBER
    final val October   = Calendar.OCTOBER
    final val November  = Calendar.NOVEMBER
    final val December  = Calendar.DECEMBER
    
    /**
     * Full date/time
     */
    def now = new Date
    
    /**
     * Date without time portion
     */
    def today = now.midnight
    
    /**
     * Tomorrow without time portion
     */
    def tomorrow  = today + 1.day
    
    /**
     * Yesterday without time portion
     */
    def yesterday = today - 1.day

}