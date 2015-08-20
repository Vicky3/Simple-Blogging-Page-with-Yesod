module Handler.BlogPostEdit where

import Import

import Yesod.Form.Nic (YesodNic, nicHtmlField)
instance YesodNic App

getBlogPostEditR :: BlogPostId -> Handler Html
getBlogPostEditR blogPostId = do
                                bPost <- runDB $ get404 blogPostId
                                (postWidget, theEnctype) <- generateFormPost (postForm bPost)
                                defaultLayout $ [whamlet|
                                  <h1>Edit Post
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
                                    ^{postWidget}
                                   <button>Submit!
                                |]

postBlogPostEditR :: BlogPostId -> Handler Html
postBlogPostEditR blogPostId = do
                                 bPost <- runDB $ get404 blogPostId
                                 ((res,_),_) <- runFormPost (postForm bPost)
                                 case res of
                                   FormSuccess changedPost -> do
                                     _ <- runDB $ replace blogPostId changedPost
                                     setMessage $ toHtml $ (blogPostTitle changedPost) <> " successfully updated."
                                     redirect $ BlogPostR blogPostId
                                   _ -> defaultLayout $ [whamlet|
                                   <h1>Sorry, something went wrong!
                                   <table>
                                      <tr>
                                        <td>
                                          <form method=get><button>Go back
                                        <td>
                                          <form method=get action=@{BlogR 1}><button>Return to main page
                                   |]

postForm :: BlogPost -> Form BlogPost
postForm bPost = renderDivs $ BlogPost
    <$> pure (blogPostAuthor bPost)
    <*> areq textField "Title: " (Just (blogPostTitle bPost))
    <*> areq nicHtmlField "Text: " (Just (blogPostText bPost))
    <*> lift (liftIO getCurrentTime)
