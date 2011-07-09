package org.oxbow.moments

import java.util.Date
import java.util.Calendar
import java.util.TimeZone
import java.text.SimpleDateFormat

sealed trait TimeAmount {
    val amount: Int
    def unary_-(): TimeAmount
    protected[moments] def addTo(date: Date): Date
    protected[moments] def setTo(date: Date): Date
}

sealed trait TimeUnit {
    protected[moments] val field: Int
}

final class TimeUnitInt(amount: Int) {

    lazy val eras: TimeAmount = CalendarAmount(Era, amount)
    lazy val era: TimeAmount = eras

    lazy val years: TimeAmount = CalendarAmount(Year, amount)
    lazy val year: TimeAmount = years

    lazy val months: TimeAmount = CalendarAmount(Month, amount)
    lazy val month: TimeAmount = months

    lazy val weeks: TimeAmount = CalendarAmount(Day, amount * 7)
    lazy val week: TimeAmount = months

    lazy val days: TimeAmount = CalendarAmount(Day, amount)
    lazy val day: TimeAmount = days

    lazy val hours: TimeAmount = CalendarAmount(Hour, amount)
    lazy val hour: TimeAmount = minutes

    lazy val minutes: TimeAmount = CalendarAmount(Minute, amount)
    lazy val minute: TimeAmount = minutes

    lazy val seconds: TimeAmount = CalendarAmount(Second, amount)
    lazy val second: TimeAmount = seconds

    lazy val milliseconds: TimeAmount = CalendarAmount(Millisecond, amount)
    lazy val millisecond: TimeAmount = milliseconds

}

final class SuperDate(date: Date) extends Ordered[Date] {

    /**
     * Returns a copy of the date with time unit added
     */
    def +(unit: TimeAmount): Date = unit.addTo(date)

    /**
     * Returns a copy of the date with time unit subtracted
     */
    def -(unit: TimeAmount): Date = (-unit).addTo(date)

    /**
     * Provides Ordered trait capabilities
     */
    def compare(that: Date): Int = if (that == null) -1 else date.compareTo(that)

    protected[moments] def withCalendar(f: Calendar => Unit): Date = {
        val c = calendar
        f(c)
        c.getTime
    }

    /**
     * Converts date to a calendar
     */
    def calendar: Calendar = {
        val c = Calendar.getInstance
        c.setTime(date)
        c
    }

    /**
     * Returns a copy of the date with provided fields set to zero
     */
    def clear(fields: TimeUnit*): Date = withCalendar(c => fields.distinct.foreach(f => c.set(f.field, 0)))

    /**
     * Returns a copy of the date with time portion set to zero
     */
    def midnight: Date = clear(Hour, Minute, Second, Millisecond)

    /**
     * Returns a copy of the date representing first day of the month
     */
    def monthBegin: Date = withCalendar(_.set(Day.field, 1)).midnight

    /**
     * Returns a copy of the date representing last day of the month
     */
    def monthEnd: Date = date.monthBegin + 1.month - 1.day

    def era = calendar.get(Era.field)
    def year = calendar.get(Year.field)
    def month = calendar.get(Month.field)
    def day = calendar.get(Day.field)
    def hour = calendar.get(Hour.field)
    def minute = calendar.get(Minute.field)
    def second = calendar.get(Second.field)

    def dayOfYear = calendar.get(Calendar.DAY_OF_YEAR)
    def dayOfWeek = calendar.get(Calendar.DAY_OF_WEEK)

    def weekOfYear = calendar.get(Calendar.WEEK_OF_YEAR)
    def weekOfMonth = calendar.get(Calendar.WEEK_OF_MONTH)

    /**
     * Formats date according to a format @see DateFormat
     */
    def format(format: String): String = (new SimpleDateFormat(format)).format(date)

    /**
     * Returns GMT date
     */
    def gmt: Date = {

        val tz = TimeZone.getDefault
        val dt = new Date(date.getTime - tz.getRawOffset)

        // back off by delta if we're in DST
        if (tz.inDaylightTime(dt)) {

            var dstDate = new Date(dt.getTime - tz.getDSTSavings)

            // check if date is not crossed into standard time
            if (tz.inDaylightTime(dstDate)) return dstDate
        }

        dt
    }

}

private[this] case class CalendarTimeUnit(val field: Int) extends TimeUnit

/**
 * Private implementation of time unit
 */
private[this] case class CalendarAmount(private val unit: TimeUnit, override val amount: Int) extends TimeAmount {

    override def addTo(date: Date): Date = date.withCalendar(_.add(unit.field, amount))

    override def setTo(date: Date): Date = date.withCalendar(_.set(unit.field, amount))

    override def unary_-(): TimeAmount = copy(amount = -amount)

}

