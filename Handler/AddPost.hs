module Handler.AddPost where

import Import

import Yesod.Form.Nic (YesodNic, nicHtmlField)
instance YesodNic App

getAddPostR :: Handler Html
getAddPostR = do
                (bPostWidget, theEnctype) <- generateFormPost aForm
                defaultLayout $ [whamlet|
                  <h1>Add a new post
                  <form method=get action=@{BlogR 1}><button>Return</button></form>    <form method=get action=@{SettingsR}><button>Settings</button></form>
                  <form method=post action=@{AddPostR} enctype=#{theEnctype}>
                    <noscript>To get a nice editor, please enable JavaScript!
                    ^{bPostWidget}
                    <button>Submit!
                  <hr>
                |]

postAddPostR :: Handler Html
postAddPostR = do
                 ((res,bPostWidget),theEnctype) <- runFormPost aForm
                 case res of
                   FormSuccess bPost -> do
                     bPostId <- runDB $ insert bPost
                     redirect $ BlogR 1
                   _ -> defaultLayout $ [whamlet|
                   <h1>Sorry, something went wrong!
                   <form method=get action=@{AddPostR}><button>Try again</button></form>    <form method=get action=@{BlogR 1}><button>Return to main page</button></form>
                   |]
                 

aForm :: Form BlogPost
aForm = renderDivs $ BlogPost
    <$> areq textField "Title: " Nothing
    <*> areq nicHtmlField "Text: " Nothing
    <*> lift (liftIO getCurrentTime)
