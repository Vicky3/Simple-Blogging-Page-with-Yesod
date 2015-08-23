module Handler.Name where

import Import

getNameR :: Handler Html
getNameR = do
             name <- runDB $ selectFirst [] [Desc BlogNameId]
             let blogName = case name of
                              Nothing -> "the standard name"
                              Just (Entity _ (BlogName n)) -> n
             (nameWidget, theEnctype) <- generateFormPost nameForm
             defaultLayout $ [whamlet|
               <h1>Change blog name
               Do you want to change the blog's name from #{blogName} to
               <form method=post enctype=#{theEnctype}>
                 ^{nameWidget}
                 <button>Change blog name
               <form method=get action=@{SettingsR}>
                 <button>NOOO!!!
             |]

postNameR :: Handler Html
postNameR = do
              ((res,_),_) <- runFormPost nameForm
              case res of
                FormSuccess name -> do
                  old <- runDB $ selectFirst [] [Asc BlogNameId]
                  _ <- case old of
                    Nothing -> do 
                                 _ <- runDB $ insert name
                                 setMessage $ "Name successfully changed."
                                 redirect $ SettingsR
                    Just (Entity a (BlogName _)) -> do 
                                                       _ <- runDB $ replace a name
                                                       setMessage $ "Name successfully changed."
                                                       redirect $ SettingsR
                  -- _ <- runDB $ replace id name
                  setMessage $ "Name successfully changed."
                  redirect $ SettingsR
                _ -> defaultLayout $ [whamlet|
                <h1>Sorry, something went wrong!
                <table>
                   <tr>
                     <td>
                       <form method=get><button>Go back
                     <td>
                       <form method=get action=@{SettingsR}><button>Return to settings.
                |]

nameForm :: Form BlogName
nameForm = renderDivs $ BlogName <$> areq textField "Newn ame: " Nothing
