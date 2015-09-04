module Handler.UserEdit where

import Import

getUserEditR :: UserId -> Handler Html
getUserEditR userId = do
                        -- get the user to edit from DB
                        user <- runDB $ get404 userId

                        -- widget to make changes
                        (userWidget, theEnctype) <- generateFormPost (userForm user)

                        -- flags for the menu
                        maid <- maybeAuthId -- if you're logged in
                        let showHome     = True
                        let showNewPost  = True
                        let showSettings = True

                        -- the output
                        defaultLayout $ do
                          [whamlet| <h1>Edit User |]
                          $(widgetFile "menuBar")
                          [whamlet|
                            <form method=post enctype=#{theEnctype}>
                              ^{userWidget}
                              <button>Submit!
                          |]

postUserEditR :: UserId -> Handler Html
postUserEditR userId = do
                         user <- runDB $ get404 userId
                         ((res,_),_) <- runFormPost (userForm user)
                         case res of
                           -- save changes into DB and return to settings
                           FormSuccess changedUser -> do
                             _ <- runDB $ replace userId changedUser
                             setMessage $ toHtml $ (userName changedUser) <> " successfully updated"
                             redirect $ SettingsR
                           -- cases FormMissing and FormFailure
                           _ -> defaultLayout $(widgetFile "failure")

userForm :: User -> Form User
userForm user = renderDivs $ User
    <$> areq emailField "Email: " (Just (userEmail user))
    <*> areq textField "Name: " (Just (userName user))
    <*> areq checkBoxField "Admin: " (Just (userAdmin user))
