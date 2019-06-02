module Main exposing (main)

import Browser
import Browser.Navigation as Nav
import GitHub exposing (Issue, Repo)
import Html exposing (..)
import Html.Attributes exposing (..)
import Http
import Json.Decode as D exposing (Decoder)
import Page.Repo
import Page.Top
import Page.User
import Route exposing (Route)
import Url
import Url.Builder



-- MAIN


main : Program () Model Msg
main =
    Browser.application
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        , onUrlChange = UrlChanged
        , onUrlRequest = LinkClicked
        }



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none



-- MODEL


type alias Model =
    { key : Nav.Key
    , page : Page
    }


type Page
    = NotFound
      -- 各ページのModelを持たせる
    | TopPage Page.Top.Model
    | UserPage Page.User.Model
    | RepoPage Page.Repo.Model


init : () -> Url.Url -> Nav.Key -> ( Model, Cmd Msg )
init flags url key =
    -- 後に画面遷移で使うためのキーを Model に持たせておく
    Model key (TopPage Page.Top.init)
        -- はじめてページを訪れたときにページの初期化を行う
        |> goTo (Route.parse url)



-- UPDATE


type Msg
    = LinkClicked Browser.UrlRequest
    | UrlChanged Url.Url
      -- 各ページのMsgを持たせる
    | TopMsg Page.Top.Msg
    | RepoMsg Page.Repo.Msg
    | UserMsg Page.User.Msg



-- | Loaded (Result Http.Error Page)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        -- （１）画面遷移のリクエストを受けたとき
        LinkClicked urlRequest ->
            -- Debug.todo "リンクがクリックされたときの挙動を実装する"
            case urlRequest of
                -- 内部リンクならブラウザのURLを更新する
                -- (SPAなので実際に表示するページはindex.htmlのまま)
                Browser.Internal url ->
                    ( model, Nav.pushUrl model.key (Url.toString url) )

                -- 外部リンクなら通常の画面遷移を行う
                Browser.External href ->
                    ( model, Nav.load href )

        -- （２）ブラウザのURLが変更されたとき
        UrlChanged url ->
            -- ページの初期化処理をヘルパー関数(goTo関数)に移譲
            goTo (Route.parse url) model

        -- （３）Repoページのメッセージが来たとき
        RepoMsg repoMsg ->
            -- 現在表示しているページが
            case model.page of
                -- RepoPageであれば
                RepoPage repoModel ->
                    -- Repoページのupdate処理を行う
                    let
                        ( newRepoModel, topCmd ) =
                            Page.Repo.update repoMsg repoModel
                    in
                    ( { model | page = RepoPage newRepoModel }
                    , Cmd.map RepoMsg topCmd
                    )

                -- RepoPageでなければ何も行わない
                _ ->
                    ( model, Cmd.none )

        -- （４）Topページのメッセージが来たとき -> 実装する
        TopMsg topMsg ->
            case model.page of
                TopPage topModel ->
                    let
                        ( newTopModel, topCmd ) =
                            Page.Top.update topMsg topModel
                    in
                    ( { model | page = TopPage newTopModel }
                    , Cmd.map TopMsg topCmd
                    )

                _ ->
                    ( model, Cmd.none )

        -- （５）Userページのメッセージが来たとき -> 実装する
        UserMsg userMsg ->
            case model.page of
                UserPage userModel ->
                    let
                        ( newUserModel, topCmd ) =
                            Page.User.update userMsg userModel
                    in
                    ( { model | page = UserPage newUserModel }
                    , Cmd.map UserMsg topCmd
                    )

                _ ->
                    ( model, Cmd.none )



{- パス（URL）に応じて各ページを初期化する -}


goTo : Maybe Route -> Model -> ( Model, Cmd Msg )
goTo maybeRoute model =
    case maybeRoute of
        -- URLに該当するURLがなかった場合はNot Foundページ
        Nothing ->
            ( { model | page = NotFound }, Cmd.none )

        -- Route.Topだった場合はトップページ
        Just Route.Top ->
            ( { model | page = TopPage Page.Top.init }
            , Cmd.none
            )

        -- Route.Userだった場合
        Just (Route.User userName) ->
            let
                ( userModel, userCmd ) =
                    Page.User.init userName
            in
            ( { model | page = UserPage userModel }
            , Cmd.map UserMsg userCmd
            )

        -- Route.Repoだった場合
        Just (Route.Repo userName projectName) ->
            -- Repoページの初期化
            let
                ( repoModel, repoCmd ) =
                    Page.Repo.init userName projectName
            in
            ( { model | page = RepoPage repoModel }
            , Cmd.map RepoMsg repoCmd
            )



-- VIEW


view : Model -> Browser.Document Msg
view model =
    { title = "My GitHub Viewer"
    , body =
        [ section [ class "hero is-primary" ]
            [ div [ class "hero-body" ]
                [ div [ class "container" ]
                    [ h1 [ class "title" ]
                        [ a [ href "/" ] [ text "My GitHub Viewer2" ]
                        ]
                    ]
                ]
            ]

        -- 場合分けしてページを表示
        , section [ class "section" ]
            [ div [ class "container" ]
                [ case model.page of
                    NotFound ->
                        viewNotFound

                    TopPage topPageModel ->
                        Page.Top.view topPageModel
                            |> Html.map TopMsg

                    UserPage userPageModel ->
                        Page.User.view userPageModel
                            |> Html.map UserMsg

                    -- 現在表示しているページがRepoPageであれば
                    RepoPage repoPageModel ->
                        -- Repoページのview関数を呼ぶ
                        Page.Repo.view repoPageModel
                            |> Html.map RepoMsg
                ]
            ]
        , footer [ class "footer" ]
            [ div [ class "content has-text-centered" ]
                [ p []
                    [ a [ href "http://i-doctor.sakura.ne.jp/font/?p=38522" ] [ text "WordPressでフリーオリジナルフォント2" ]
                    ]
                ]
            ]
        ]
    }



{- NotFound ページ -}


viewNotFound : Html msg
viewNotFound =
    text "not found"
