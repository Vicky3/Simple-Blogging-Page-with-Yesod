module Handler.UserEdit where

import Import

getUserEditR :: UserId -> Handler Html
getUserEditR userId = do
                        user <- runDB $ get404 userId
                        (userWidget, theEnctype) <- generateFormPost (userForm user)
                        defaultLayout $ [whamlet|
                          <h1>Edit User
                          <table>
                            <tr>
                              <td>
                                <form method=get action=@{AuthR LogoutR}>
                                  <button>Logout
                              <td>
                                <form method=get action=@{BlogR 1}>
                                  <button>Home
                              <td>
                                <form method=get action=@{AddPostR}>
                                  <button>New Post
                              <td>
                                <form method=get action=@{SettingsR}>
                                  <button>Settings
                          <form method=post enctype=#{theEnctype}>
                            ^{userWidget}
                           <button>Submit!
                        |]

postUserEditR :: UserId -> Handler Html
postUserEditR userId = do
                         user <- runDB $ get404 userId
                         ((res,_),_) <- runFormPost (userForm user)
                         case res of
                           FormSuccess changedUser -> do
                             _ <- runDB $ replace userId changedUser
                             setMessage $ toHtml $ (userName changedUser) <> " successfully updated"
                             redirect $ SettingsR
                           _ -> defaultLayout $ [whamlet|
                           <h1>Sorry, something went wrong!
                           <table>
                              <tr>
                                <td>
                                  <form method=get><button>Go back
                                <td>
                                  <form method=get action=@{BlogR 1}><button>Return to main page
                           |]

userForm :: User -> Form User
userForm user = renderDivs $ User
    <$> areq emailField "Email: " (Just (userEmail user))
    <*> areq textField "Name: " (Just (userName user))
    <*> areq checkBoxField "Admin: " (Just (userAdmin user))
