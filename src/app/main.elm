module Soccer where 

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Signal exposing (..)
import String
import StartApp.Simple as StartApp
import List.Extra
import Json.Decode as Json
import Debug
import Dict exposing (..)


-- wire the entire application together
main =
   StartApp.start { model = emptyModel, view = view, update = update }



-- MODEL
type alias Model =
    { teams: Dict Int Team      
    , teamId: Int
    , newTeamName: String
    , newGroupName: String
    , teamsSelected: List Int
    }
    
emptyModel: Model
emptyModel =
    { teams = Dict.singleton 0 (newTeam "test" "A" 0) 
    , teamId = 0
    , newTeamName = ""
    , newGroupName = ""
    , teamsSelected = []
    }
    
type alias Team = 
    {
        name: String
        , id: Int
        , group: String
        , selected: Bool
    }
    
newTeam: String -> String -> Int -> Team
newTeam nameInput groupInput idInput =
    {
        name = nameInput
        , id = idInput
        , group = groupInput
        , selected = False
    }





-- UPDATE
type Action
    = AddTeam String String
    | RemoveTeam Int
    | EnterTeamName String
    | EnterGroupName String
    | TeamClicked Int


update: Action -> Model -> Model
update action model =
    case action of
        TeamClicked teamId ->
            {-
                model |
                    teams = List.map (\t -> 
                        if t.id == teamId
                            then {t | selected = not t.selected}
                            else {t | selected = t.selected})
                        model.teams,
                    teamsSelected = List.filterMap (\t ->
                        if t.selected == True
                            then Just(t.id)
                            else Nothing)
                        model.teams
            -}
            model            
            
        EnterTeamName nameInput ->
            {
                model |
                    newTeamName = nameInput
            }
        EnterGroupName groupInput ->
            {
                model |
                    newGroupName = groupInput
            }
        AddTeam nameInput groupInput ->
            {-
                model |
                    teamId = model.teamId + 1,
                    teams =
                        if String.isEmpty nameInput
                            then model.teams
                            else Dict.union model.teams (Dict.singleton model.teamId (newTeam nameInput groupInput model.teamId )),
                    newTeamName = "",
                    newGroupName = ""
            -}
            model
        RemoveTeam idInput ->
            {-
                model | teams = List.filter (\team -> team.id /= idInput) model.teams
            -}
            model
            

-- VIEW


view: Address Action -> Model -> Html
view address model =
    let 
        distinctGroups = List.Extra.unique (List.map .group model.teams.values)
    in
    div []
    [
        
--        renderGroups distinctGroups
--        , div []
--        [ text (String.join "," distinctGroups) ]
{-        
        div [ style [("border", "1px solid black")
            , ("height", "200px")
            , ("width", "300px") ] 
        ]
        [ ul
            [ ]
            ( List.map (teamItem address) model.teams.values )
        ]
-}        
        div [ style [("border", "1px solid black")
            , ("height", "200px")
            , ("width", "300px") ] 
        ]
        [
            input
            [ placeholder "Group..."
            , on "input" targetValue (Signal.message address << EnterGroupName)
            , value model.newGroupName
            , maxlength 1
            , style [("width", "50px"), ("margin-right", "2px")]
            ]
            [] 
            , input
            [ placeholder "Team name..."
            , on "input" targetValue (Signal.message address << EnterTeamName)
            , value model.newTeamName
            , onEnter address (AddTeam model.newTeamName model.newGroupName)
            , style [("margin-right", "2px")]
            ]
            []
            , button
            [ onClick address (AddTeam model.newTeamName model.newGroupName)]
            [ text "Add" ]
        ]
    ]
     
teamItem : Address Action -> Team -> Html
teamItem address team =
    li
      [ onDoubleClick address (TeamClicked team.id) 
      , classList [ ("selected", team.selected) ]
      , style [ ("-moz-user-select", "none"), ("-webkit-user-select", "none"), ("-ms-user-select", "none"), ("user-select", "none"), ("-o-user-select", "none") ] 
      ]
      [ text team.name ]


renderGroups : List String -> Html
renderGroups groups =
    ul
      [ ]
        ( List.map (\element -> li [] [ text element ]) groups )





-- onEnter Support for input fields

onEnter : Address a -> a -> Attribute
onEnter address value =
    on "keydown"
      (Json.customDecoder keyCode is13)
      (\_ -> Signal.message address value)


is13 : Int -> Result String ()
is13 code =
  if code == 13 then Ok () else Err "not the right key code"
