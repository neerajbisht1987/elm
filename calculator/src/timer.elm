module Main exposing (..)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Browser exposing (sandbox)
import Time exposing (Time,second)

type Status = Running | Expired

type alias Model =
    {
        expirationTime:Time
        ,remainingTime:Time
        ,status:Status
    }

    

initModel : Model
initModel = {
    expirationTime = parseTime "Mar 14 2017 13:05:00"
    , remainingTime = 0
    , status = Running
}

parseTime : String -> Time
parseTime string = 
    Date.fromString string
    |> Result.Default (Date.fromTime  0)
    |> Date.toTime

type Msg = CurrentTime Time

update : Msg -> Model  ->( Model ,Cmd Msg)

update msg model =
    case msg of 
        CurrentTime now ->
            let 
                remainingTime =
                    model.expirationTime - now
                status =
                    if remainingTime < 0 
                    then status Expired
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

viewRemainingTime :Time -> List (Html Msg)
viewRemainingTime time = 
    List.map viewTimePeriod (timePeriods t)

viewTimePeriod :(String, String) -> Html Msg
viewTimePeriod (period amount) = 
    div [class "time-period"]
    [
        span [class "amount"][text amount]
        ,span [class "period"][text period]
    ]


timePeriods :Time ->List(String,String)
timePeriods time =
    let seconds = 
            floor (t/1000) % 60
        minutes = 
            floor (t/1000 / 60) % 60
        hour =
            floor(t/1000/ 60 * 60)% 24 
        addLeadingZeros n = 
            String.padLeft 2 '0' (toString n)
    in 
        [days,hours,minutes,seconds]
        |> List.map addLeadingZeros
        |> List.map2 (,) ["days","hours","minutes","seconds "]

subscriptions : Model -> Sub Msg
subscriptions model =
        Time.every second CurrentTime


main : Program () Model Msg
main =  Browser.sandbox
        { init = initProperty
        , view = view
        , update = update
        ,subscriptions = subscriptions
        }