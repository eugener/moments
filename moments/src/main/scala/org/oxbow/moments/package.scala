package org.oxbow

import java.util.Calendar
import java.util.Date

package object moments {

    implicit def date2super( date: Date ) = new SuperDate( date )
    implicit def int2timeAmount( amount: Int ): TimeUnitInt = new TimeUnitInt( amount )
    
    final val Era: TimeUnit         = CalendarTimeUnit(Calendar.ERA)
    final val Year: TimeUnit        = CalendarTimeUnit(Calendar.YEAR)
    final val Month: TimeUnit       = CalendarTimeUnit(Calendar.MONTH)
    final val Day: TimeUnit         = CalendarTimeUnit(Calendar.DAY_OF_MONTH)
    final val Hour: TimeUnit        = CalendarTimeUnit(Calendar.HOUR)
    final val Minute: TimeUnit      = CalendarTimeUnit(Calendar.MINUTE)
    final val Second: TimeUnit      = CalendarTimeUnit(Calendar.SECOND)
    final val Millisecond: TimeUnit = CalendarTimeUnit(Calendar.MILLISECOND)
    
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
