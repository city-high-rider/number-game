-- the idea in this module is to convert our url routes from strings into
-- custom types, so that we are less likely to make a mistake while doing 
-- routing
module Route exposing (..)

-- This contains the pages that we can route to
type Route 
    = NotFoundPage
    | HomePage
