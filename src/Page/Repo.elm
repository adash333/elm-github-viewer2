module Page.Repo exposing (Model, Msg, init, update, view)

import GitHub exposing (Issue, Repo)
import Html exposing (..)
import Html.Attributes exposing (..)
import Http


-- MODEL


type alias Model =
    { userName : String
    , projectName : String
    , state : State
    }

type State
    = Init
    | Loaded (List Issue)
    | Error Http.Error

init : String -> String -> (Model, Cmd Msg)
init userName projectName =
    -- ページの初期化
    -- 最初のModelを作ると同時に、ページの表示に必要なデータをHTTPで取得する
    ( Model userName projectName Init
    , GitHub.getIssues 
        GotIssues
        userName
        projectName
    )


-- UPDATE


type Msg
    = GotIssues (Result Http.Error (List Issue))

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    -- 'init'でのHTTPリクエストの結果が得られたらModelを更新する
    case msg of
        GotIssues (Ok issues) ->
            ( { model | state = Loaded issues }, Cmd.none )
        
        GotIssues (Err err) ->
            ( { model | state = Error err }, Cmd.none )


-- VIEW


view : Model -> Html Msg
view model =
    case model.state of
        Init ->
            text "Loading..."

        Loaded issues ->
            ul [] (List.map (viewIssue model.userName model.projectName) issues)

        Error e ->
            text (Debug.toString e)
            -- ここのDebugは、productionのときにはコメントアウトまたは変更する。

viewIssue : String -> String -> Issue -> Html Msg
viewIssue userName projectName issue =
    li []
        [ span [] [ text ("[" ++ issue.state ++ "]") ]
        , a [ href (GitHub.issueUrl userName projectName issue.number), target "_blank"] [text ("#" ++ String.fromInt issue.number), text issue.title]
        ]