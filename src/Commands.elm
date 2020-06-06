port module Commands exposing (..)

import Json.Decode as JD
import Json.Encode as JE


type Msg
    = Registered JE.Value
    | GotUserList JE.Value
    | JoinedGameChannel JE.Value


port addNewUser : JE.Value -> Cmd msg


port registered : (JE.Value -> msg) -> Sub msg


port gotUserList : (JE.Value -> msg) -> Sub msg


port joinedGameChannel : (JE.Value -> msg) -> Sub msg


port startNewGame : JE.Value -> Cmd msg

