module Main exposing (..)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Browser exposing (sandbox)
import Time 
import Time exposing (Month(..), utc)
import Time.Extra as Time

type Status = Running | Expired

type alias Model =
    {
        expirationTime:Time.Posix
        ,remainingTime:Time.Posix
        ,status:Status
    }
--parseTime : String -> Time.Posix
--parseTime string = 
--    Time.Parts string |> Time.partsToPosix utc
--   Date.fromString string
--    |> Result.Default (Date.fromTime  0)
--    |> Date.toTime


initModel : Model
initModel = {
    expirationTime=(Time.Parts 2017 Mar 14 13 5 0 |> Time.partsToPosix utc),
    remainingTime=0,
    status=Running}



type Msg = CurrentTime Time.Posix

update : Msg -> Model  ->( Model ,Cmd Msg)

update msg model =
    case msg of 
        CurrentTime now ->
            let 
                remainingTime =
                    model.expirationTime - now
                status =
                    if remainingTime < 0 
                    then Expired
                    else Running
            in 
                ( {
                    model |remainingTime=remainingTime,
                    status = status},
                    Cmd.none)

view : Model ->Html Msg
view model = 
    case model.status of 
        Running ->
            div [] (viewRemainingTime model.remainingTime)
        Expired ->
            h3 [][text "Expired"]

viewRemainingTime :Time.Posix -> List (Html Msg)
viewRemainingTime time = 
    List.map viewTimePeriod (timePeriods time)

viewTimePeriod :(String, String) -> Html Msg
viewTimePeriod (period ,amount) = 
    div [class "time-period"]
    [
        span [class "amount"][text amount]
        ,span [class "period"][text period]
    ]


timePeriods :Time.Posix ->List(String,String)
timePeriods time =
    let seconds = Time.toSecond utc time            
        minutes = Time.toMinute utc time
        hours = Time.toHour utc time
        days = Time.toDay utc time
        addLeadingZeros n = 
            String.padLeft 2 '0' (Debug.toString n)
    in 
        [days,hours,minutes,seconds]
        |> List.map addLeadingZeros
        |> List.map2 ("," ) ["days","hours","minutes","seconds "]

subscriptions : Model -> Sub Msg
subscriptions model =
        Time.every Time.second CurrentTime


main : Program () Model Msg
main =  Browser.sandbox
        { init = initModel
        , view = view
        , update = update
        ,subscriptions = subscriptions
        }