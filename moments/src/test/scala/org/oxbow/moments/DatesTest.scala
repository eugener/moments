package org.oxbow.moments

import org.scalatest.FunSuite
import org.scalatest.junit.AssertionsForJUnit
import org.junit.Assert._
import org.junit.Test
import java.util.Date
import Dates._
import java.util.Calendar
import org.scalatest.junit.ShouldMatchersForJUnit

class DatesTest extends AssertionsForJUnit with ShouldMatchersForJUnit {

    @Test def clearFields = {
        
        val dt = date( hour = 1, minute = 30, second = 15 ).clear( Calendar.HOUR, Calendar.MINUTE ) 
        
        assert( dt.hour == 0, "The hour is not cleaned" ) 
        assert( dt.minute == 0, "The minute is not cleaned" )
        assert( dt.second != 0, "The second is 0, but should not be" )
        
    }
    
    @Test def formatDate = {
        
        val s = date( year=2011, month=Calendar.JULY, day=2 ).format("yyyy-MM-dd")
        assert( s == "2011-07-02", "Incorrect date formatting" )
        
    }
    
    @Test def dateAddtion = {
        
        val d = date( month=Calendar.JULY ) + 1.month
        assert ( d == date( month=Calendar.AUGUST ), "Incorrect date addition" )
        
    }
    
     @Test def dateSubtraction = {
        
        val d = date( year=2011 ) - 11.years
        assert ( d == date( year=2000 ), "Incorrect date substraction" )
        
    }
     
     @Test def dateComparison = {
         
         assert( yesterday < today )
         assert( tomorrow > today )
         
         assert( tomorrow >= today )
         assert( today >= today )
         
     }
    
}