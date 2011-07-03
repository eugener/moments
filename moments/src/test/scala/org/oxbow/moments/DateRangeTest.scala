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
        	tomorrow.combine(yesterday)
        }
    }
    
    @Test def includeDate = {
        
        val range = yesterday.combine(null)
        assert( range.includes(Some(today)), "Date inclusion test failed"  )
        assert( range.includes(Some(yesterday)), "Date inclusion test failed"  )
        assert( range.includes(Some(yesterday - 5.days)) == false, "Date inclusion test failed"  )
        
    }
    
    @Test def resize = {
        
        val dr1 = DateRange( Some(date()), Some(date()) )
        val dr2 = dr1.expand( 1.day, 1.day )
        assert( dr2.begin.get == yesterday && dr2.end.get == tomorrow )
        
    }
    
}