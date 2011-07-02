package org.oxbow.moments

import org.junit._
import org.junit.Assert._
import java.util.Calendar
import Dates._

@Test
class DatesTest {

    @Test
    def testDayDuration() = {
        
        var c: Calendar = today.asCalendar
        
        c.add( Calendar.DAY_OF_MONTH, -1)
        
        println( "................ " + (today - 8.day) )
        println( "................ " + c.getTime )
        
//        assertEquals( c.getTime, (today - 1.day) )
        
//        c = today.asCalendar
//        c.add( Calendar.DAY_OF_MONTH, 8 )
//        assertEquals( c.getTime, today + 8.days )
        
    }

}


