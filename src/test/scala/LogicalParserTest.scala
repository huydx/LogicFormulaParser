class LogicalParserTest extends org.specs2.mutable.Specification {
  val parser = new LogicalParser()

  "binary operator normal case" >> {
    "true and true" >> {
      parser.parse("true and true") must_== true
    }

    "true and false" >> {
      parser.parse("true and false") must_== false
    }

    "false and false" >> {
      parser.parse("true and false") must_== false
    }

    "true or true" >> {
      parser.parse("true or true") must_== true
    }

    "false or false" >> {
      parser.parse("false or false") must_== false
    }

    "true or false" >> {
      parser.parse("true or false") must_== true
    }
  }

  "not operator" >> {
    "not true" >> {
      parser.parse("not true") must_== false
    }

    "not false" >> {
      parser.parse("not false") must_== true
    }

    "not (true and false)" >> {
      parser.parse("not (true and false)") must_== true
    }
  }

  "complex nest operator" >> {
    "true and (true or (true and not false))" >> {
      parser.parse("true and (true or (true and not false))") must_== true
    }

    "false and (true or (true and not false))" >> {
      parser.parse("false and (true or (true and not false))") must_== false
    }

    "false and (true and (not true and not false))" >> {
      parser.parse("false and (true and (not true and not false))") must_== false
    }
  }

  "invalid operator" >> {
    "true and" >> {
      parser.parse("true and") must throwA[Exception]
    }

    "True and" >> {
      parser.parse("True and Foo") must throwA[Exception]
    }
  }
}
