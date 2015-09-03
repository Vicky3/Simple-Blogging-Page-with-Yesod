module Handler.UserDelete where

import Import

getUserDeleteR :: UserId -> Handler Html
getUserDeleteR userId = do
                          -- find user in DB
                          user <- runDB $ get404 userId
                          -- ask for confirmation (short enough - not necessary to put in template file)
                          defaultLayout $ [whamlet|
                            <h1>Delete User
                            Do you really want to delete #{userName user} with email #{userEmail user}?
                            <table>
                              <tr>
                                <td>
                                  <form method=post action=@{UserDeleteR userId}>
                                    <button>Yes
                                <td>
                                  <form method=get action=@{SettingsR}>
                                    <button>No
                          |]

postUserDeleteR :: UserId -> Handler Html
postUserDeleteR userId = do
                           -- delete user and return to settings
                           runDB $ delete userId
                           setMessage "Successfully deleted"
                           redirect $ SettingsR
