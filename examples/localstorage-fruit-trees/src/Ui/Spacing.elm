module Ui.Spacing exposing
    ( Edges
    , borders
    , edges
    , l
    , m
    , s
    , xl
    , xs
    , xxs
    )


xxs : number
xxs =
    5


xs : number
xs =
    10


s : number
s =
    20


m : number
m =
    40


l : number
l =
    80


xl : number
xl =
    144



-- Utils


type alias Edges unit =
    { top : unit
    , right : unit
    , bottom : unit
    , left : unit
    }


edges : Edges number
edges =
    { top = 0
    , right = 0
    , bottom = 0
    , left = 0
    }


type alias Borders unit =
    { topLeft : unit
    , topRight : unit
    , bottomLeft : unit
    , bottomRight : unit
    }


borders : Borders number
borders =
    { topLeft = 0
    , topRight = 0
    , bottomLeft = 0
    , bottomRight = 0
    }
