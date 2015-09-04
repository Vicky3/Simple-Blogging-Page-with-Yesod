module Handler.Settings where

import Import

getSettingsR :: Handler Html
getSettingsR = do
                 -- get a list of users from DB
                 users <- runDB $ selectList [] [Asc UserId]

                 -- widget to add new user
                 (userWidget, theEnctype) <- generateFormPost userForm

                 -- flags for the menu
                 maid <- maybeAuthId -- if you're logged in
                 let showHome     = True
                 let showNewPost  = True
                 let showSettings = False

                 -- the output
                 defaultLayout $ do
                   [whamlet| <h1>Settings |]
                   $(widgetFile "menuBar")
                   $(widgetFile "settings")

postSettingsR :: Handler Html
postSettingsR = do
                  ((res,_),_) <- runFormPost userForm
                  case res of
                    FormSuccess user -> do
                      -- check uniqueness of email
                      checked <- runDB $ checkUnique user
                      case checked of
                        -- email is only allowed once
                        Just _ -> setMessage $ "Email address is already in use!"
                        Nothing -> do 
                          -- insert new user in DB
                          _ <- runDB $ insert user
                          setMessage $ toHtml $ (userName user) <> " successfully created."
                      -- return to settings
                      redirect $ SettingsR

                    -- cases FormMissing and FormFailure
                    _ -> defaultLayout $(widgetFile "failure")

userForm :: Form User
userForm = renderDivs $ User
    <$> areq emailField "Email: " Nothing
    <*> areq textField "Name: " Nothing
    <*> areq checkBoxField "Admin: " Nothing
