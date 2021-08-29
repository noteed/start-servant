{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Prototype.Pages.Home where

import Text.Blaze.Html5 (Html, (!))
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A

import Prototype.Types

--------------------------------------------------------------------------------
thisPage = pageUrl thisFile

thisFile = "Prototype/Pages/Home.hs"

pageUrl path =
  H.a ! A.href (H.toValue $ sourceUrl ++ path) $
    "This page on GitHub"

repositoryUrl = "https://github.com/noteed/start-servant"

sourceUrl = repositoryUrl ++ "/tree/main/src/"

--------------------------------------------------------------------------------
homePage :: Maybe Profile -> Html

homePage (Just Profile {..}) = H.div $ do
  H.div $ do
    "Hi "
    H.toHtml name
    "."

  H.ul $ do
    H.li $ H.a ! A.href (H.toValue $ "/" ++ namespace) $ "Your profile"
    H.li $ H.a ! A.href "/settings/profile" $ "Settings"
    H.li $ H.a ! A.href "/database" $ "Database"

  thisPage

homePage Nothing = H.div $ do
  H.p $ do
    "Welcome to "
    H.code "start-servant"
    ". This is an example application to demonstrate how Servant and STM can "
    "be used to prototype a classical three-tier web application (where STM "
    "replaces a traditional relational database)."

  H.p $
    "Please sign in."

  thisPage
