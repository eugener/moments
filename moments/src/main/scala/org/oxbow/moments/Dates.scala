package org.oxbow.moments

import java.util.Date
import java.util.Calendar
import java.text.SimpleDateFormat
import java.util.TimeZone

object Dates {
    
    implicit def date2x( date: Date ) = new {
        
        /**
         * Returns a copy of the date with time unit added
         */
    	def +( unit: TimeUnit ): Date = unit.applyTo( date )
    	
    	/**
    	 * Returns a copy of the date with time unit subtracted
    	 */
    	def -( unit: TimeUnit ): Date = unit.applyTo( date, true )
    	
    	def <( other: Date ): Boolean  = date.before(other) 
    	def <=( other: Date ): Boolean = date.before(other) || date.equals(other)

    	def >( other: Date ): Boolean  = date.after(other) 
    	def >=( other: Date ): Boolean = date.after(other) || date.equals(other)

    	def calendar: Calendar = {
    	   val c = Calendar.getInstance
    	   c.setTime( date )
    	   c 
    	}
    	
    	/**
    	 * Returns a copy of the date with provided fields cleared
    	 */
    	def clear( fields: Int* ): Date = {
    	    val c: Calendar = calendar
    	    fields.foreach( c.set( _, 0))
    	    c.getTime
    	}

    	/**
    	 * Returns a copy of the date with time portion cleared
    	 */
    	def midnight: Date = clear( Calendar.HOUR, Calendar.MINUTE, Calendar.SECOND, Calendar.MILLISECOND )
    	
    	def era    = calendar.get(Calendar.ERA)
    	def year   = calendar.get(Calendar.YEAR)
    	def month  = calendar.get(Calendar.MONTH)
    	def day    = calendar.get(Calendar.DAY_OF_MONTH)
    	def hour   = calendar.get(Calendar.HOUR)
    	def minute = calendar.get(Calendar.MINUTE)
    	def second = calendar.get(Calendar.SECOND)
    	
    	def dayOfYear   = calendar.get(Calendar.DAY_OF_YEAR)
    	def dayOfWeek   = calendar.get(Calendar.DAY_OF_WEEK)
    	
    	def weekOfYear  = calendar.get(Calendar.WEEK_OF_YEAR)
    	def weekOfMonth = calendar.get(Calendar.WEEK_OF_MONTH)

    	/**
    	 * Formats date according to a format @see DateFormat
    	 */
    	def format( format: String ): String = new SimpleDateFormat(format).format(date) 
    	
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
        protected[Dates] def applyTo(date: Date, negate: Boolean = false): Date
    }
    
    
    /**
     * Private implementation of time unit
     */
    private[this] case class TimeUnitImpl( private val field: Int, override val amount: Int) extends TimeUnit {
        
        override def applyTo(date: Date, negate: Boolean = false ): Date = {
            val c: Calendar = date.calendar
            c.add( field, if (negate) -amount else amount )
            c.getTime
        }
        
    }
    
    
    implicit def int2timeUnit( amount: Int ) = new {

        lazy val eras: TimeUnit = TimeUnitImpl(Calendar.ERA, amount)
        lazy val era : TimeUnit = eras
        
        lazy val years: TimeUnit = TimeUnitImpl(Calendar.YEAR, amount)
        lazy val year : TimeUnit = years
        
        lazy val months: TimeUnit = TimeUnitImpl(Calendar.MONTH, amount)
        lazy val month : TimeUnit = months
        
        lazy val days: TimeUnit = TimeUnitImpl(Calendar.DAY_OF_MONTH, amount)
        lazy val day : TimeUnit = days
        
        lazy val minutes: TimeUnit = TimeUnitImpl(Calendar.MINUTE, amount)
        lazy val minute: TimeUnit  = minutes
        
        lazy val seconds: TimeUnit = TimeUnitImpl(Calendar.SECOND, amount)
        lazy val second: TimeUnit  = seconds
        
        lazy val milliseconds: TimeUnit = TimeUnitImpl(Calendar.MILLISECOND, amount)
        lazy val millisecond: TimeUnit  = milliseconds
        
    }
    
    /** 
     * Creates a date. 
     * Era, year, month and day default to current date values 
     * Hour, minute, second and millisecond default to zero 
     */
    def date( era: Int = -1, 
              year: Int = -1, 
              month: Int = -1,
              day: Int = -1, 
              hour: Int = 0,
              minute: Int = 0,
              second: Int = 0, 
              millisecond: Int = 0 ): Date = {
        
        val c: Calendar = new Date().calendar
        
        if ( era >= 0 ) c.set( Calendar.ERA, era )
        if ( year >= 0 ) c.set( Calendar.YEAR, year )
        if ( month >= 0 ) c.set( Calendar.MONTH, month )
        if ( day >= 0 ) c.set( Calendar.DAY_OF_MONTH, day )
        c.set( Calendar.HOUR, hour )
        c.set( Calendar.MINUTE, minute )
        c.set( Calendar.SECOND, second )
        c.set( Calendar.MILLISECOND, millisecond )
        
        c.getTime
    }
    
    /**
     * Full date and time
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