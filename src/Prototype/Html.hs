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
  "Please "
  H.a ! A.href "/login" $ "log in"
  "."


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
  H.h1 "Login page"
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


--------------------------------------------------------------------------------
namespaceIndex :: Profile -> [TodoList] -> Html
namespaceIndex profile lists = H.div $ do
  H.h1 "Namespace index page"
  H.toHtml (namespace profile)
  H.ul $
    forM_ lists $ \TodoList {..} -> do
      H.li (H.toHtml tlName)
