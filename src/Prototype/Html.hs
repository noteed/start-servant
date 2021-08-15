{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Prototype.Html where

import Control.Monad (forM_)
import Data.Aeson (FromJSON, ToJSON)
import Text.Blaze (ToMarkup(toMarkup))
import Text.Blaze.Html5 (Html, (!))
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A

import Prototype.Types


--------------------------------------------------------------------------------
htmlProfile Profile{..} = do
  H.div $ do
    H.div $ do
      "Display name: "
      H.toHtml name
    H.div $ do
      "Username: "
      H.toHtml namespace
    H.div $ do
      "Email: "
      H.toHtml email

instance ToMarkup Profile where
  toMarkup  = htmlProfile


--------------------------------------------------------------------------------
document mprofile title body = H.docTypeHtml $ do
  H.head $ do
    H.title $ H.toHtml title
  H.body $ do
    page mprofile body

document' title body = H.docTypeHtml $ do
  H.head $ do
    H.title $ H.toHtml title
  H.body $ do
    page' body


--------------------------------------------------------------------------------
page mprofile body = do
  nav mprofile
  body
  H.footer $ H.code $ "start-servant"

page' body = do
  shortNav
  body
  H.footer $ H.code $ "start-servant"


--------------------------------------------------------------------------------
nav :: Maybe Profile -> Html

nav (Just Profile {..}) = H.div $
  H.ul $ do
    H.li $ H.a ! A.href "/" $ "Home"
    H.li $ H.toHtml namespace
    H.li $ H.a ! A.href "/settings/profile" $ "Your profile"
    H.li $ H.a ! A.href "/logout" $ "Sign out"

nav Nothing = H.div $
  H.ul $ do
    H.li $ H.a ! A.href "/" $ "Home"
    H.li $ H.a ! A.href "/login" $ "Sign in"

shortNav =
  H.ul $ do
    H.li $ H.a ! A.href "/" $ "Home"

--------------------------------------------------------------------------------
homePage :: Maybe Profile -> Html

homePage (Just Profile {..}) = H.div $ do
  H.div $ do
    "Hi "
    H.toHtml name
    "."

  H.ul $ do
    H.li $ H.a ! A.href "/settings/profile" $ "Your profile"
    H.li $ H.a ! A.href "/database" $ "Database"

homePage Nothing = H.div $ do
  H.p $ do
    "Welcome to "
    H.code "start-servant"
    ". This is an example application to demonstrate how Servant and STM can "
    "be used to prototype a classical three-tier web application (where STM "
    "replaces a traditional relational database)."

  H.p $
    "Please sign in."


--------------------------------------------------------------------------------
profilePage :: Profile -> Html

profilePage profile = H.div $ do
  H.h1 "Profile page"
  H.toHtml profile


--------------------------------------------------------------------------------
loginPage :: Maybe Profile -> Html

loginPage (Just Profile {..}) = H.div $
  H.toHtml ("Hi " ++ namespace ++ ", you're already logged in.")

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
    H.div $ do
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
  H.toHtml (namespace profile)
  H.ul $
    forM_ lists $ \TodoList {..} -> do
      H.li (H.toHtml tlName)


--------------------------------------------------------------------------------
todoListIndex :: Profile -> TodoList -> Html
todoListIndex profile list = H.div $ do
  H.h1 "Namespace index page"
  H.toHtml (namespace profile)
  H.toHtml (tlName list)
  H.ul $
    forM_ (tlItems list) $ \TodoItem {..} -> do
      H.li $ do
        H.toHtml tiDescription
        " - "
        H.toHtml (show tiState)
