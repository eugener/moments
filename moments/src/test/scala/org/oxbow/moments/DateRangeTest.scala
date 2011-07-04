package org.oxbow.moments

import org.scalatest.FunSuite
import org.scalatest.junit.AssertionsForJUnit
import org.junit.Assert._
import org.junit.Test
import java.util.Date
import Moments._
import java.util.Calendar
import org.scalatest.junit.ShouldMatchersForJUnit

class DateRangeTest extends AssertionsForJUnit with ShouldMatchersForJUnit {
    
    @Test def dateCheck: Unit = {
        intercept[IllegalArgumentException] {
        	new DateRange(tomorrow,yesterday)
        }
    }
    
    @Test def includesDate = {
        
        val range = DateRange( Option(yesterday), None )
        assert( range.includes(today), "Date inclusion test failed"  )
        assert( range.includes(yesterday), "Date inclusion test failed"  )
        assert( !range.includes(yesterday - 5.days ), "Date inclusion test failed"  )
        
    }
    
    @Test def expand = {
        
        val dr1 = new DateRange( today )
        val dr2 = dr1.expand( 1.day )
        assert( dr2.begin.get == yesterday && dr2.end.get == tomorrow )
        
    }
    
    @Test def shiftBack = {
        
        val dr1 = new DateRange( today )
        val dr2 = dr1 << 1.day
        assert( dr2.begin.get == yesterday && dr2.end.get == yesterday )
        
    }
    
    @Test def shiftForward = {
        
        val dr1 = new DateRange( today )
        val dr2 = dr1 >> 1.day
        assert( dr2.begin.get == tomorrow && dr2.end.get == tomorrow )
        
    }
    
}
