module AllTests exposing (..)

import ElmTest exposing (..)
import DummiesTest


main : Program Never
main =
    runSuiteHtml DummiesTest.dummiesSuite
