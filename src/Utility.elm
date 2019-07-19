module Utility exposing (textHtml)

import Html
import Html.Parser
import Html.Parser.Util


textHtml : String -> List (Html.Html msg)
textHtml t =
    case Html.Parser.run t of
        Ok nodes ->
            Html.Parser.Util.toVirtualDom nodes

        Err _ ->
            []
