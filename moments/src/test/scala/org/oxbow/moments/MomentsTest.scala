package org.oxbow.moments

import org.junit.Test
import org.scalatest.junit.AssertionsForJUnit
import org.scalatest.junit.ShouldMatchersForJUnit

import Moments._

class MomentsTest extends AssertionsForJUnit with ShouldMatchersForJUnit { 

   
    
   @Test def clearFields = {
        
        val dt = dateTime( 1.hour, 30.minutes, 15.seconds ).clear( Hour, Minute )
        
        assert( dt.hour == 0, "The hour is not cleaned" ) 
        assert( dt.minute == 0, "The minute is not cleaned" )
        assert( dt.second != 0, "The second is 0, but should not be" )
        
    }
    
    @Test def formatDate = {
        val s = dateTime( 2011.year, July.month, 2.day ).format("yyyy-MM-dd")
        assert( s == "2011-07-02", "Incorrect date formatting" )
    }
    
    @Test def dateAddtion = {
        assert ( date( July.month ) + 1.month == date( August.month ), "Incorrect month addition" )
        assert ( date( 1.day ) + 2.weeks == date( 15.day ), "Incorrect week addition" )
    }
    
    @Test def dateSubtraction = {
        assert ( date( 2011.year ) - 11.years == date( 2000.year ), "Incorrect year subtraction" )
        assert ( date( 22.day ) - 3.weeks == date( 1.day ), "Incorrect week subtaction" )
    }
     
    @Test def dateComparison = {
         
         assert( yesterday < today )
         assert( tomorrow > today )
         
         assert( tomorrow >= today )
         assert( today >= today )
         
     }
     
     @Test def monthBegin = {
         
         val y = 2011.year
         val m = July.month
         val d = dateTime( y, m, 3.day ).monthBegin
         assert( d.year == y.amount && d.month == m.amount && d.day == 1, "Beginning of month is calculated incorrectly" )
         
     }

     @Test def monthEnd = {
         
         val y = 2011.year
         val m = July.month
         val d = dateTime( y, m, 3.day ).monthEnd
         assert( d.year == y.amount && d.month == m.amount && d.day == 31, "End of month is calculated incorrectly" )
         
     }
     
}
