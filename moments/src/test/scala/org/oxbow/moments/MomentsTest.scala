package org.oxbow.moments

import org.junit.Test
import org.scalatest.junit.AssertionsForJUnit
import org.scalatest.junit.ShouldMatchersForJUnit

import Moments._
import java.util.Calendar

class MomentsTest extends AssertionsForJUnit with ShouldMatchersForJUnit { 

   @Test def clearFields = {
        
        val dt = date( hour = 1, minute = 30, second = 15 ).clear( Hour, Minute ) 
        
        assert( dt.hour == 0, "The hour is not cleaned" ) 
        assert( dt.minute == 0, "The minute is not cleaned" )
        assert( dt.second != 0, "The second is 0, but should not be" )
        
    }
    
    @Test def formatDate = {
        val s = date( year=2011, month=July, day=2 ).format("yyyy-MM-dd")
        assert( s == "2011-07-02", "Incorrect date formatting" )
    }
    
    @Test def dateAddtion = {
        assert ( date( month=July ) + 1.month == date( month=August ), "Incorrect month addition" )
        assert ( date( day=1 ) + 2.weeks == date( day=15 ), "Incorrect week addition" )
    }
    
    @Test def dateSubtraction = {
        assert ( date( year=2011 ) - 11.years == date( year=2000 ), "Incorrect year subtraction" )
        assert ( date( day=22 ) - 3.weeks == date( day=1 ), "Incorrect week subtaction" )
    }
     
     @Test def dateComparison = {
         
         assert( yesterday < today )
         assert( tomorrow > today )
         
         assert( tomorrow >= today )
         assert( today >= today )
         
     }
     
     @Test def monthBegin = {
         
         val y = 2011
         val m = July
         val d = date( year=y, month=m, day=3 ).monthBegin
         assert( d.year == y && d.month == m && d.day == 1 )
         
     }

     @Test def monthEnd = {
         
         val y = 2011
         val m = July
         val d = date( year=y, month=m, day=3 ).monthEnd
         assert( d.year == y && d.month == m && d.day == 31 )
         
     }
     
}