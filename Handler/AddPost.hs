module Handler.AddPost where

import Import

import Yesod.Form.Nic (YesodNic, nicHtmlField)
instance YesodNic App

getAddPostR :: Handler Html
getAddPostR = do
                (bPostWidget, theEnctype) <- generateFormPost bPostForm
                defaultLayout $ [whamlet|
                  <h1>Add a new post
                  <table>
                      <tr>
                        <td>
                          <form method=get action=@{BlogR 1}>
                            <button>Home
                        <td>
                          <form method=get action=@{SettingsR}>
                            <button>Settings
                  <form method=post enctype=#{theEnctype}>
                    <noscript>
                      <b>To get a nice editor, please enable JavaScript!
                    ^{bPostWidget}
                    <button>Submit!
                  <hr>
                |]

postAddPostR :: Handler Html
postAddPostR = do
                 ((res,_),_) <- runFormPost bPostForm
                 case res of
                   FormSuccess bPost -> do
                     bPostId <- runDB $ insert bPost
                     setMessage $ toHtml $ (blogPostTitle bPost) <> " successfully created"
                     redirect $ BlogPostR bPostId
                   _ -> defaultLayout $ [whamlet|
                   <h1>Sorry, something went wrong!
                   <table>
                      <tr>
                        <td>
                          <form method=get><button>Go back
                        <td>
                          <form method=get action=@{BlogR 1}><button>Return to main page
                   |]
                 

bPostForm :: Form BlogPost
bPostForm = renderDivs $ BlogPost
    <$> areq textField "Title: " Nothing
    <*> areq nicHtmlField "Text: " Nothing
    <*> lift (liftIO getCurrentTime)
