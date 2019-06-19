module Main exposing (..)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Browser exposing (sandbox)



type Opr 
    = Add|Sub|Mul|Div|Non

type alias Model = {value:Float, opr:Opr}

initProperty : Model
initProperty = {
    value=0,
    opr=Non}


-- Update
type Msg = Operatorset Opr|Cal Float |Clr

update : Msg -> Model -> Model
update msg model =
    case msg of
        Operatorset opr->
            {model | opr = opr}

        Cal val->
         case model.opr of 
            Add -> {model| value = model.value + val}
            Sub -> {model| value = model.value - val}
            Mul -> {model| value = model.value * val}
            Div -> {model| value = model.value / val}
            Non -> {model| value = val}
        Clr -> {model| value =0 , opr=Non} 

view : Model -> Html Msg
view model =
    div[]
    [ h1 [] [ text "Score Keeper" ]
    ,form13 model
    ,form46 model
    ,form79 model
    ,formop1 model
    ,formop2 model
    ,h3 []
            [ text ("Calculated Value : " ++ (Debug.toString model.value)) ]
    ]
form13 : Model -> Html Msg
form13 model =
    div [][
        button[
            type_ "button"
            ,onClick (Cal 1)
        ][ text "1" ]
        ,button[
            type_ "button"
            ,onClick (Cal 2)
        ][ text "2" ]
        ,button[
            type_ "button"
            ,onClick (Cal 3)
        ][ text "3" ]
    ]

form46 : Model -> Html Msg
form46 model =
    Html.form [][
        button[
            type_ "button"
            ,onClick (Cal 4)
        ][ text "4" ]
        ,button[
            type_ "button"
            ,onClick (Cal 5)
        ][ text "5" ]
        ,button[
            type_ "button"
            ,onClick (Cal 6)
        ][ text "6" ]
    ]

form79 : Model -> Html Msg
form79 model =
    Html.form [][
        button[
            type_ "button"
            ,onClick (Cal 7)
        ][ text "7" ]
        ,button[
            type_ "button"
            ,onClick (Cal 8)
        ][ text "8" ]
        ,button[
            type_ "button"
            ,onClick (Cal 9)
        ][ text "9" ]
    ]

formop1 : Model -> Html Msg
formop1 model =
    Html.form [][
        button[
            type_ "button"
            ,onClick (Cal 0)
        ][ text "0" ]
        ,button[
            type_ "button"
            ,onClick (Operatorset Add)
        ][ text "+" ]
        ,button[
            type_ "button"
            ,onClick (Operatorset Sub)
        ][ text "-" ]
        
    ]

formop2 : Model -> Html Msg
formop2 model =
    Html.form [][
        button[
            type_ "button"
            ,onClick Clr
        ][ text "C" ]
        ,button[
            type_ "button"
            ,onClick (Operatorset Mul)
        ][ text "*" ]
        ,button[
            type_ "button"
            ,onClick (Operatorset Div)
        ][ text "/" ]
        
    ]

main : Program () Model Msg
main =  Browser.sandbox
        { init = initProperty
        , view = view
        , update = update
        }