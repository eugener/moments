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
    	def +( unit: TimeAmount ): Date = unit.addTo( date )
    	
    	/**
    	 * Returns a copy of the date with time unit subtracted
    	 */
    	def -( unit: TimeAmount ): Date = (-unit).addTo( date )
    	
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
    	def clear( fields: TimeUnit* ): Date = withCalendar( c => fields.distinct.foreach( f =>c.set( f.field, 0)) ) 

    	/**
    	 * Returns a copy of the date with time portion set to zero
    	 */
    	def midnight: Date = clear( Hour, Minute, Second, Millisecond )
    	
    	/**
    	 * Returns a copy of the date representing first day of the month
    	 */
    	def monthBegin: Date = withCalendar( _.set( Day.field, 1 )).midnight 
    	
    	/**
    	 * Returns a copy of the date representing last day of the month
    	 */
    	def monthEnd: Date = date.monthBegin + 1.month - 1.day
    	
    	
    	def era    = calendar.get(Era.field)
    	def year   = calendar.get(Year.field)
    	def month  = calendar.get(Month.field)
    	def day    = calendar.get(Day.field)
    	def hour   = calendar.get(Hour.field)
    	def minute = calendar.get(Minute.field)
    	def second = calendar.get(Second.field)
    	
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
     * Abstract time unit amount 
     */
    sealed trait TimeAmount {
        val amount: Int
        def addTo(date: Date): Date
        def setTo(date: Date): Date
        def unary_-(): TimeAmount
    }
    
    sealed trait TimeUnit {
        protected[Moments] val field: Int
    }
    
    private[this] case class TimeUnitImpl( protected[Moments] val field: Int ) extends TimeUnit
    
    
    /**
     * Private implementation of time unit
     */
    private[this] case class TimeAmountImpl( private val unit: TimeUnit, override val amount: Int) extends TimeAmount {
        
        override def addTo(date: Date): Date = date.withCalendar( _.add( unit.field, amount ) )
        
        override def setTo(date: Date): Date = date.withCalendar( _.set( unit.field, amount ) )
        
        override def unary_-(): TimeAmount = copy( amount = -amount )
        
    }
    
    final class TimeUnitInt( amount: Int ) {

        lazy val eras: TimeAmount = TimeAmountImpl(Era, amount)
        lazy val era : TimeAmount = eras
        
        lazy val years: TimeAmount = TimeAmountImpl(Year, amount)
        lazy val year : TimeAmount = years
        
        lazy val months: TimeAmount = TimeAmountImpl(Month, amount)
        lazy val month : TimeAmount = months

        lazy val weeks: TimeAmount = TimeAmountImpl(Day, amount*7 )
        lazy val week : TimeAmount = months
        
        lazy val days: TimeAmount = TimeAmountImpl(Day, amount)
        lazy val day : TimeAmount = days
        
        lazy val hours: TimeAmount = TimeAmountImpl(Hour, amount)
        lazy val hour: TimeAmount  = minutes
        
        lazy val minutes: TimeAmount = TimeAmountImpl(Minute, amount)
        lazy val minute: TimeAmount  = minutes
        
        lazy val seconds: TimeAmount = TimeAmountImpl(Second, amount)
        lazy val second: TimeAmount  = seconds
        
        lazy val milliseconds: TimeAmount = TimeAmountImpl(Millisecond, amount)
        lazy val millisecond: TimeAmount  = milliseconds
        
    }
    
    final val Era: TimeUnit         = TimeUnitImpl(Calendar.ERA)
    final val Year: TimeUnit        = TimeUnitImpl(Calendar.YEAR)
    final val Month: TimeUnit       = TimeUnitImpl(Calendar.MONTH)
    final val Day: TimeUnit         = TimeUnitImpl(Calendar.DAY_OF_MONTH)
    final val Hour: TimeUnit        = TimeUnitImpl(Calendar.HOUR)
    final val Minute: TimeUnit      = TimeUnitImpl(Calendar.MINUTE)
    final val Second: TimeUnit      = TimeUnitImpl(Calendar.SECOND)
    final val Millisecond: TimeUnit = TimeUnitImpl(Calendar.MILLISECOND)
    
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
    def dateTime( units: TimeAmount* ): Date = makeDate(units)
    
    /**
     * Creates date modified using provided units with no time portion
     */
    def date( units: TimeAmount* ): Date = makeDate(units).midnight

    
    private def makeDate( units: Iterable[TimeAmount]) = units.foldLeft(now)((d,unit) => unit.setTo(d))
    
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
