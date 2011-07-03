package org.oxbow.moments

import java.util.Calendar

import org.junit.Test
import org.scalatest.junit.AssertionsForJUnit
import org.scalatest.junit.ShouldMatchersForJUnit

import Dates._

class DatesTest extends AssertionsForJUnit with ShouldMatchersForJUnit {

    @Test def clearFields = {
        
        val dt = date( hour = 1, minute = 30, second = 15 ).clear( Calendar.HOUR, Calendar.MINUTE ) 
        
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
        assert ( date( year=2011 ) - 11.years == date( year=2000 ), "Incorrect year substraction" )
        assert ( date( day=22 ) - 3.weeks == date( day=1 ), "Incorrect week subtaction" )
    }
     
     @Test def dateComparison = {
         
         assert( yesterday < today )
         assert( tomorrow > today )
         
         assert( tomorrow >= today )
         assert( today >= today )
         
     }
     
     @Test def monthBegin = {
         
         val year = 2011
         val month = 7
         val d = date( year=year, month=month, day=3 ).monthBegin
         assert( d.year == year && d.month == month && d.day == 1 )
         
     }

     @Test def monthEnd = {
         
         val year = 2011
         val month = 7
         val d = date( year=year, month=month, day=3 ).monthEnd
         assert( d.year == year && d.month == month && d.day == 31 )
         
     }
     
}