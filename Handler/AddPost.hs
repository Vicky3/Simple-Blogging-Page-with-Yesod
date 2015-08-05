module Handler.AddPost where

import Import
import Yesod.Form.Nic (YesodNic, nicHtmlField)

instance YesodNic App

getAddPostR :: Handler Html
getAddPostR = do
                (bPostWidget, theEnctype) <- generateFormPost aForm
                defaultLayout $ [whamlet|
                  <h1>Add a new post
                  <form method=get action=@{BlogR}><button>Return</form>    <form method=get action=@{SettingsR}><button>Settings</form>
                  <form method=post action=@{AddPostR} enctype=#{theEnctype}>
                    ^{bPostWidget}
                    <button>Submit!
                |]

postAddPostR :: Handler Html
postAddPostR = error "Not yet implemented: postAddPostR"

aForm :: Form BlogPost
aForm = renderDivs $ BlogPost
  <$> areq textField "Title" Nothing
  <*> areq nicHtmlField "Text" Nothing
  <*> lift (liftIO getCurrentTime)
