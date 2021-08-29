{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Prototype.Html where

import Prototype.Types.NonEmptyText (nonEmptyToTextCoerce)
import Control.Monad (forM_)
import Text.Blaze.Html5 (Html, (!))
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A

import Prototype.Types



--------------------------------------------------------------------------------
document mprofile title body = H.docTypeHtml $ do
  H.head $
    H.title $ H.toHtml title
  H.body $
    page mprofile body

document' title body = H.docTypeHtml $ do
  H.head $
    H.title $ H.toHtml title
  H.body $
    page' body


--------------------------------------------------------------------------------
page mprofile body = do
  nav mprofile
  body
  H.footer $ do
    H.code "start-servant"
    " - "
    H.a ! A.href "https://github.com/noteed/start-servant" $ "View on GitHub"


page' body = do
  shortNav
  body
  H.footer $ H.code "start-servant"


--------------------------------------------------------------------------------
nav :: Maybe Profile -> Html

nav (Just Profile {..}) = H.div $
  H.ul $ do
    H.li $ H.a ! A.href "/" $ "Home"
    H.li $ H.toHtml namespace
    H.li $ H.a ! A.href (H.toValue $ "/" <> namespace) $ "Your profile"
    H.li $ H.a ! A.href "/settings/profile" $ "Settings"
    H.li $ H.a ! A.href "/logout" $ "Sign out"

nav Nothing = H.div $
  H.ul $ do
    H.li $ H.a ! A.href "/" $ "Home"
    H.li $ H.a ! A.href "/login" $ "Sign in"

shortNav =
  H.ul $
    H.li $ H.a ! A.href "/" $ "Home"


--------------------------------------------------------------------------------
profilePage :: Profile -> Html

profilePage profile = H.div $ do
  H.h1 "Profile page"
  H.toHtml profile


--------------------------------------------------------------------------------
loginPage :: Maybe Profile -> Html

loginPage (Just Profile {..}) = H.div $
  H.toHtml ("Hi " <> namespace <> ", you're already logged in.")

loginPage Nothing = H.div $ do
  H.h1 "Sign in"
  H.form
    ! A.action "/login"
    ! A.method "POST" $ do

    H.div $ do
      H.label ! A.for "username" $ "Username:"
      H.input
        ! A.type_ "text"
        ! A.name "username"
        ! A.id "username"
        ! A.required "required"
    H.div $ do
      H.label
        ! A.for "password" $ "Password:"
      H.input
        ! A.type_ "password"
        ! A.name "password"
        ! A.id "password"
        ! A.required "required"
    H.div $
      H.button
        ! A.type_ "submit" $ "Login"


--------------------------------------------------------------------------------
databaseIndex = H.div $ do
  H.h1 "Database index"

  H.p "This page lists all the objects manipulated in this prototype."

  H.ul $ do
    H.li $ H.a ! A.href "/a/sessions" $ "Sessions"
    H.li $ H.a ! A.href "/a/profiles" $ "Profiles"
    H.li $ H.a ! A.href "/a/lists" $ "Todo lists"


--------------------------------------------------------------------------------
namespaceIndex :: Profile -> [TodoList] -> Html
namespaceIndex profile lists = H.div $ do
  H.h1 "Namespace index page"
  H.div . H.code $
    H.toHtml (namespace profile)
  H.ul $
    forM_ lists $ \TodoList {..} ->
      H.li $ H.a ! A.href (H.toValue $ "/" <> nonEmptyToTextCoerce (namespace profile) <> "/" <> tlName) $
        H.toHtml tlName


--------------------------------------------------------------------------------
todoListIndex :: Profile -> TodoList -> Html
todoListIndex profile list' = H.div $ do
  H.h1 "Namespace index page"
  H.div . H.code $ do
    H.toHtml (namespace profile)
    " / "
    H.toHtml (tlName list')
  H.ul $
    forM_ (tlItems list') $ \TodoItem {..} ->
      H.li $ do
        H.toHtml tiDescription
        " - "
        H.toHtml @Text (show tiState)
