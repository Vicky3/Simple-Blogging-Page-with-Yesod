module Handler.Name where

import Import

getNameR :: Handler Html
getNameR = do
             -- get old name
             name <- runDB $ selectFirst [] [Desc BlogNameId]
             let blogName = case name of
                              Nothing -> "the standard name"
                              Just (Entity _ (BlogName n)) -> n

             -- widget to change blog name
             (nameWidget, theEnctype) <- generateFormPost nameForm

             -- the output
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
                  -- check if a name was given before
                  old <- runDB $ selectFirst [] [Asc BlogNameId]
                  _ <- case old of
                    Nothing -> do 
                                 -- no name - so insert the new name (and return to settings)
                                 _ <- runDB $ insert name
                                 setMessage $ "Name successfully set."
                                 redirect $ SettingsR
                    Just (Entity a (BlogName _)) -> do 
                                 -- there was a name - overwrite it! (and return to settings)
                                                      _ <- runDB $ replace a name
                                                      setMessage $ "Name successfully changed."
                                                      redirect $ SettingsR

                  -- I think there is really no way to get here - but Haskell wants it -.-
                  setMessage $ "This should not happen!!!"
                  redirect $ SettingsR

                -- cases FormMissing and FormFailure
                _ -> defaultLayout $(widgetFile "failure")

nameForm :: Form BlogName
nameForm = renderDivs $ BlogName <$> areq textField "New name: " Nothing
