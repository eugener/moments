package org.oxbow.moments

import org.scalatest.FunSuite
import org.scalatest.junit.AssertionsForJUnit
import org.junit.Assert._
import org.junit.Test
import java.util.Date
import Dates._
import java.util.Calendar
import org.scalatest.junit.ShouldMatchersForJUnit

class DateRangeTest extends AssertionsForJUnit with ShouldMatchersForJUnit {

    val range = yesterday.combine(null)
    
    @Test def includeDate = {
        
        assert( range.includes(Some(today)), "Date inclusion test failed"  )
        assert( range.includes(Some(yesterday)), "Date inclusion test failed"  )
        assert( range.includes(Some(yesterday - 5.days)) == false, "Date inclusion test failed"  )
        
    }
    
    
}