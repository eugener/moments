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
    	def +( unit: TimeUnit ): Date = unit.increment( date )
    	
    	/**
    	 * Returns a copy of the date with time unit subtracted
    	 */
    	def -( unit: TimeUnit ): Date = unit.negate.increment( date )
    	
    	/**
    	 * Converts date to a calendar
    	 */
    	def asCalendar: Calendar = {
	        val c = Calendar.getInstance
	        c.setTime( date )
	        c
	    }
    	
    	/**
    	 * Returns a copy of the date with provided fields cleared
    	 */
    	def clear( fields: Int* ): Date = {
    	    val c = asCalendar
    	    fields.foreach( c.set( _, 0))
    	    c.getTime
    	}

    	/**
    	 * Returns a copy of the date with time portion cleared
    	 */
    	def midnight: Date = clear( Calendar.HOUR, Calendar.MINUTE, Calendar.SECOND, Calendar.MILLISECOND )
    	
    	def era    = asCalendar.get(Calendar.ERA)
    	def year   = asCalendar.get(Calendar.YEAR)
    	def month  = asCalendar.get(Calendar.MONTH)
    	def day    = asCalendar.get(Calendar.DAY_OF_MONTH)
    	def hour   = asCalendar.get(Calendar.HOUR)
    	def minute = asCalendar.get(Calendar.MINUTE)
    	def second = asCalendar.get(Calendar.SECOND)
    	
    	def dayOfYear   = asCalendar.get(Calendar.DAY_OF_YEAR)
    	def dayOfWeek   = asCalendar.get(Calendar.DAY_OF_WEEK)
    	
    	def weekOfYear  = asCalendar.get(Calendar.WEEK_OF_YEAR)
    	def weekOfMonth = asCalendar.get(Calendar.WEEK_OF_MONTH)

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

    }
    
    /**
     * Abstract time unit
     */
    sealed trait TimeUnit {
        
        val field: Int
        val amount: Int
        val negate: TimeUnit
        
        def increment(date: Date): Date = {
            val c: Calendar = date.asCalendar
            c.add(field, amount)
            c.getTime
        }
    }
    
    /**
     * Private implementation of time unit
     */
    private[this] case class TimeUnitImpl( override val field: Int, override val amount: Int) extends TimeUnit {
        lazy val negate = copy(amount= -amount) // use named parameter - only amount changing
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
        
        val c = new Date().asCalendar
        
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
    def now   = new Date
    
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