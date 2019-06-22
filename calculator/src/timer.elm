module Main exposing (..)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Browser exposing (sandbox)
import Time exposing (Month, utc)
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


initModel : (Model, Cmd Msg)
initModel = ({
    expirationTime=
        Time.Parts 2019 Time.Jul 14 13 5 0 0 |> Time.partsToPosix utc,
    remainingTime=Time.millisToPosix 0,
    status=Running}, Cmd.none)



type Msg = CurrentTime Time.Posix


remainingTimefn : Time.Posix -> Time.Posix -> Time.Posix
remainingTimefn exprTime now =
    
    let
        expryear =  Debug.log "exprYear" (Time.toYear utc exprTime)
        exprhour =  Debug.log "exprhour" (Time.toHour utc exprTime)
        exprMin =  Debug.log "exprMin" (Time.toMinute utc exprTime)
        exprSec =  Debug.log "exprSec" (Time.toSecond utc exprTime)
        exprMon =  Debug.log "exprMon" (Time.toMonth utc exprTime)
        
        nowyear =  Debug.log "nowYear" (Time.toYear utc now)
        nowhour =  Debug.log "nowhour" (Time.toHour utc now)
        nowMin =  Debug.log "nowMin" (Time.toMinute utc now)
        nowSec =  Debug.log "nowSec" (Time.toSecond utc now)
        nowMon =  Debug.log "nowMon" (Time.toMonth utc now)

        days =
            if Time.toYear utc exprTime >=  Time.toYear utc now &&
               --Time.toMonth utc exprTime >= Time.toMonth utc now &&
               Time.toDay utc exprTime >= Time.toDay utc now
            then
                Time.toDay utc exprTime - Time.toDay utc now
            else
                0
        hours = if Time.toYear utc exprTime >=  Time.toYear utc now &&
                   --Time.toMonth utc exprTime >= Time.toMonth utc now &&
                   Time.toDay utc exprTime >= Time.toDay utc now 
                then
                     24 - Time.toHour utc now
                else
                    0
        mins =if Time.toYear utc exprTime >=  Time.toYear utc now &&
                 --Time.toMonth utc exprTime >= Time.toMonth utc now &&
                 Time.toDay utc exprTime >= Time.toDay utc now &&
                  Time.toHour utc exprTime >= Time.toHour utc now 
                then
                    60 - Time.toMinute utc now
              else 0      
        secs= if Time.toYear utc exprTime >=  Time.toYear utc now &&
                 --Time.toMonth utc exprTime >= Time.toMonth utc now &&
                 Time.toDay utc exprTime >= Time.toDay utc now &&
                 Time.toHour utc exprTime >= Time.toHour utc now &&
                 Time.toMinute utc exprTime >= Time.toMinute utc now 
                then
                    60 - Time.toSecond utc now
                else
                    0
       in
       Debug.log "Time Value" 
        (Time.Parts (Time.toYear utc now) (Time.toMonth utc now) days hours mins secs 0 )|> Time.partsToPosix utc

update : Msg -> Model  ->( Model ,Cmd Msg)

update msg model =
    case msg of 
        CurrentTime now ->
            
            let 
                remainingTime =
                    remainingTimefn model.expirationTime now
                status =
                    if  Time.toMillis utc remainingTime < 0 
                    then Expired
                    else Running
            in 
                ( {
                    model |remainingTime=remainingTime,
                    status = status},
                    Cmd.none)

view : Model -> Html Msg
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
        hours = Time.toHour utc  time
        days = Time.toDay utc time
        addLeadingZeros n = 
            String.padLeft 2 '0' (Debug.toString n)
    in 
        [days,hours,minutes,seconds]
        |> List.map addLeadingZeros
        |> List.map2 (\a b ->(a,b)) ["days","hours","minutes","seconds "]

subscriptions : Model -> Sub Msg
subscriptions model =
        Time.every 10 CurrentTime


main : Program () Model Msg
main=Browser.element
        {init=\()->initModel
        ,view=view
        ,update=update
        ,subscriptions=subscriptions        
        }