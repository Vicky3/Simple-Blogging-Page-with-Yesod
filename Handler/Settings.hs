module Handler.Settings where

import Import

getSettingsR :: Handler Html
getSettingsR = do
                 users <- runDB $ selectList [] [Asc UserId]
                 (userWidget, theEnctype) <- generateFormPost userForm
                 defaultLayout $ [whamlet|
                   <h1>Settings
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
                   <h2>Edit users
                   <table>
                     <tr>
                       <th>Id
                       <th>Name
                       <th>Email
                       <th>Admin?
                       <th>
                       <th>
                     $forall Entity userId (User email name admin) <- users
                       <tr>
                         <td>#{toPathPiece userId}
                         <td>#{name}
                         <td>#{email}
                         <td>
                           $if admin
                             Admin
                         <td><a href=@{UserEditR userId}>Edit</a>
                         <td><a href=@{UserDeleteR userId}>Delete</a>
                   <h2>Add new user
                   <form method=post enctype=#{theEnctype}>
                     ^{userWidget}
                     <button>Submit!
                   <hr>
                 |]

postSettingsR :: Handler Html
postSettingsR = do
                  ((res,_),_) <- runFormPost userForm
                  case res of
                    FormSuccess user -> do
                      checked <- runDB $ checkUnique user
                      case checked of
                        Just _ -> setMessage $ "Email address is already in use!"
                        Nothing -> do 
                          _ <- runDB $ insert user
                          setMessage $ toHtml $ (userName user) <> " successfully created."
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

userForm :: Form User
userForm = renderDivs $ User
    <$> areq emailField "Email: " Nothing
    <*> areq textField "Name: " Nothing
    <*> areq checkBoxField "Admin: " Nothing
