module Handler.UserDelete where

import Import

getUserDeleteR :: UserId -> Handler Html
getUserDeleteR userId = do
                          user <- runDB $ get404 userId
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
                           runDB $ delete userId
                           setMessage "Successfully deleted"
                           redirect $ SettingsR
