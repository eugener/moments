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
    	
    	/**
    	 * Provides Ordered trait capabilities
    	 */
    	def compare(that: Date): Int = if (that==null) -1 else date.compareTo( that )
    	
        def withCalendar( f: Calendar => Unit ): Date = {
            val c = calendar
            f( c )
            c.getTime
        }
    	
    	/**
    	 * Converts date to a calendar
    	 */
    	def calendar: Calendar = {
    	   val c = Calendar.getInstance
    	   c.setTime( date )
    	   c 
    	}
    	
    	/**
    	 * Returns a copy of the date with provided fields set to zero
    	 */
    	def clear( fields: Int* ): Date = withCalendar( c => fields.foreach( c.set( _, 0)) ) 

    	/**
    	 * Returns a copy of the date with time portion set to zero
    	 */
    	def midnight: Date = clear( Hour, Minute, Second, Millisecond )
    	
    	/**
    	 * Returns a copy of the date representing first day of the month
    	 */
    	def monthBegin: Date = withCalendar( _.set( Day, 1 )).midnight 
    	
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
        
        override def addTo(date: Date): Date = date.withCalendar( _.add( field, amount ) )
        
        override def setTo(date: Date): Date = date.withCalendar( _.set( field, amount ) )
        
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
     * Creates date/time, modified using provided units
     */
    def dateTime( units: TimeUnit* ): Date = units.foldLeft(now)((d,unit) => unit.setTo(d))
    
    /**
     * Creates date modified using provided units with no time portion
     */
    def date( units: TimeUnit* ): Date = units.foldLeft(now)((d,unit) => unit.setTo(d)).midnight

    
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